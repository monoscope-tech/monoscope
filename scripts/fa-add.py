#!/usr/bin/env python3
"""Add a FontAwesome icon to the sprite sheet.

Usage:
  python3 scripts/fa-add.py arrow-right solid
  python3 scripts/fa-add.py circle-check regular
  python3 scripts/fa-add.py chart-network light    # Pro-only style/icon

Fetches from the FontAwesome Pro API first (using FONTAWESOME_PRO_TOKEN from
env or .env), falling back to the free 6.7.2 CDN only if the token is absent
or the icon isn't in Pro, so Pro-only icons/styles need no manual steps.
"""
import sys, re, os, json, tempfile, urllib.request, urllib.error

FA_VERSION = "6.7.2"
# sprite filename (== STYLE arg) -> FontAwesome (family, style) for the Pro API
FAMILY_STYLE = {
    "solid": ("classic", "solid"),
    "regular": ("classic", "regular"),
    "light": ("classic", "light"),
    "thin": ("classic", "thin"),
    "duotone": ("duotone", "solid"),
    "brands": ("classic", "brands"),
}


def load_pro_token():
    tok = os.environ.get("FONTAWESOME_PRO_TOKEN")
    if tok:
        return tok
    try:
        for line in open(".env"):
            if line.strip().startswith("FONTAWESOME_PRO_TOKEN="):
                return line.strip().split("=", 1)[1].strip()
    except FileNotFoundError:
        pass
    return None


def fetch_free(name, style):
    url = f"https://use.fontawesome.com/releases/v{FA_VERSION}/svgs/{style}/{name}.svg"
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    try:
        with urllib.request.urlopen(req) as r:
            return r.read().decode()
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return None
        raise


def fetch_pro(name, style):
    token = load_pro_token()
    if not token:
        return None
    ua = {"User-Agent": "Mozilla/5.0"}  # api.fontawesome.com is behind Cloudflare; blocks default UA (err 1010)
    # API token -> short-lived access token
    req = urllib.request.Request(
        "https://api.fontawesome.com/token", data=b"",
        headers={"Authorization": f"Bearer {token}", **ua})
    with urllib.request.urlopen(req) as r:
        access = json.load(r)["access_token"]
    family, fstyle = FAMILY_STYLE.get(style, ("classic", style))
    query = ('query($q:String!,$v:String!){search(version:$v,query:$q,first:15)'
             '{id svgs{familyStyle{family style} html}}}')
    body = json.dumps({"query": query, "variables": {"q": name, "v": FA_VERSION}}).encode()
    req = urllib.request.Request(
        "https://api.fontawesome.com", data=body,
        headers={"Authorization": f"Bearer {access}", "Content-Type": "application/json", **ua})
    with urllib.request.urlopen(req) as r:
        data = json.load(r)
    for icon in ((data.get("data") or {}).get("search") or []):
        if icon["id"] != name:
            continue
        for s in icon["svgs"]:
            fs = s["familyStyle"]
            if fs["family"] == family and fs["style"] == fstyle:
                return s["html"]
    return None


def main():
    if len(sys.argv) < 3:
        print("usage: fa-add.py ICON STYLE", file=sys.stderr)
        sys.exit(1)
    name, style = sys.argv[1], sys.argv[2]
    target = f"static/public/assets/svgs/fa-sprites/{style}.svg"

    content = open(target).read() if os.path.exists(target) else \
        '<svg xmlns="http://www.w3.org/2000/svg" style="display:none">\n</svg>'
    if f'id="{name}"' in content:
        print(f"icon '{name}' already in {target}")
        return

    svg = fetch_pro(name, style)  # Pro is a superset of free; prefer it
    src = "Pro API"
    if svg is None:
        svg = fetch_free(name, style)
        src = "free CDN"
    if svg is None:
        print(f"icon '{name}' not found in style '{style}' (Pro API + free CDN). "
              f"Check the name/style at fontawesome.com; ensure FONTAWESOME_PRO_TOKEN is set for Pro icons.",
              file=sys.stderr)
        sys.exit(1)

    m = re.search(r'viewBox="([^"]*)"', svg)
    if not m:
        print("no viewBox found in downloaded SVG", file=sys.stderr)
        sys.exit(1)
    viewbox = m.group(1)
    inner = re.sub(r'<svg[^>]*>|</svg>|<!--.*?-->', '', svg, flags=re.DOTALL).strip()
    symbol = f'  <symbol id="{name}" viewBox="{viewbox}">\n    {inner}\n  </symbol>'
    new_content = content.replace('</svg>', symbol + '\n</svg>', 1)

    dir_ = os.path.dirname(os.path.abspath(target))
    os.makedirs(dir_, exist_ok=True)
    with tempfile.NamedTemporaryFile('w', dir=dir_, delete=False) as tmp:
        tmp.write(new_content)
        tmp_path = tmp.name
    os.replace(tmp_path, target)
    print(f"added '{name}' to {target} (via {src})")
    print(f'use: faSprite_ "{name}" "{style}" "w-4 h-4"')


if __name__ == "__main__":
    main()
