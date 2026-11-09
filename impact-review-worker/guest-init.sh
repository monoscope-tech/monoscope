touch /etc/hosts
for entry in '127.0.0.1 localhost' '::1 localhost'; do
  grep -Fxq "$entry" /etc/hosts || printf '%s\n' "$entry" >> /etc/hosts
done
# asciinema (proof recordings) needs POSIX semaphores.
mkdir -p /dev/shm && mountpoint -q /dev/shm || mount -t tmpfs -o mode=1777,nosuid,nodev,size=64m tmpfs /dev/shm
