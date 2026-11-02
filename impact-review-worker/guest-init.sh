touch /etc/hosts
for entry in '127.0.0.1 localhost' '::1 localhost'; do
  grep -Fxq "$entry" /etc/hosts || printf '%s\n' "$entry" >> /etc/hosts
done
