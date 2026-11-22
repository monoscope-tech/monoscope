#!/usr/bin/env bash
# Installs disk-guard on the CapRover host. Alerts read ALERT_WEBHOOK_URL (or legacy SLACK_WEBHOOK_URL)
# from /etc/default/disk-guard (mode 0600); existing ballast in /home/ubuntu/disk-reserve is migrated in place.
set -euo pipefail
HOST=${HOST:-ubuntu@captain.s.past3.tech}
cd "$(dirname "$0")"
tar cf - disk-guard.sh disk-guard.service disk-guard.timer | ssh "$HOST" '
  set -euo pipefail
  tmp=$(mktemp -d)
  trap '\''rm -rf "$tmp"'\'' EXIT
  tar xf - -C "$tmp"
  sudo install -o root -g root -m 755 "$tmp/disk-guard.sh" /usr/local/bin/disk-guard.sh
  sudo install -o root -g root -m 644 "$tmp/disk-guard.service" "$tmp/disk-guard.timer" /etc/systemd/system/
  sudo install -d -o root -g root -m 755 /var/lib/disk-guard/ballast
  if sudo compgen -G "/home/ubuntu/disk-reserve/ballast-*.bin" >/dev/null; then
    sudo mv -n /home/ubuntu/disk-reserve/ballast-*.bin /var/lib/disk-guard/ballast/
    sudo chown root:root /var/lib/disk-guard/ballast/ballast-*.bin
    sudo chmod 600 /var/lib/disk-guard/ballast/ballast-*.bin
    rmdir /home/ubuntu/disk-reserve 2>/dev/null || true
  fi
  if sudo test -e /etc/default/disk-guard; then
    sudo chown root:root /etc/default/disk-guard
    sudo chmod 600 /etc/default/disk-guard
  fi
  sudo systemctl daemon-reload
  sudo systemctl enable --now disk-guard.timer
  sudo systemctl start disk-guard.service
  systemctl list-timers disk-guard.timer --no-pager
  journalctl -t disk-guard -n 5 --no-pager
'
