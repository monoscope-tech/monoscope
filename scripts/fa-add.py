#!/usr/bin/env python3
"""Add a FontAwesome icon to the sprite sheet.

Usage:
  python3 scripts/fa-add.py arrow-right solid
  python3 scripts/fa-add.py circle-check regular
"""
import sys, re, os, tempfile, urllib.request, urllib.error

if len(sys.argv) < 3:
    print("usage: fa-add.py ICON STYLE", file=sys.stderr)
    sys.exit(1)

name, style = sys.argv[1], sys.argv[2]
target = f"static/public/assets/svgs/fa-sprites/{style}.svg"

content = open(target).read()
if f'id="{name}"' in content:
    print(f"icon '{name}' already in {target}")
    sys.exit(0)

url = f"https://use.fontawesome.com/releases/v6.7.2/svgs/{style}/{name}.svg"
req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
try:
    with urllib.request.urlopen(req) as r:
        svg = r.read().decode()
except urllib.error.HTTPError as e:
    print(f"icon '{name}' not found in style '{style}' (HTTP {e.code})", file=sys.stderr)
    sys.exit(1)

m = re.search(r'viewBox="([^"]*)"', svg)
if not m:
    print(f"no viewBox found in downloaded SVG", file=sys.stderr)
    sys.exit(1)

viewbox = m.group(1)
inner = re.sub(r'<svg[^>]*>|</svg>|<!--.*?-->', '', svg, flags=re.DOTALL).strip()
symbol = f'  <symbol id="{name}" viewBox="{viewbox}">\n    {inner}\n  </symbol>'
new_content = content.replace('</svg>', symbol + '\n</svg>', 1)
dir_ = os.path.dirname(os.path.abspath(target))
with tempfile.NamedTemporaryFile('w', dir=dir_, delete=False) as tmp:
    tmp.write(new_content)
    tmp_path = tmp.name
os.replace(tmp_path, target)
print(f"added '{name}' to {target}")
print(f'use: faSprite_ "{name}" "{style}" "w-4 h-4"')
