#!/usr/bin/env node
// Add a FontAwesome 6.7.2 icon: node scripts/fa-add.mjs ICON STYLE.
// Pro uses FONTAWESOME_PRO_TOKEN from the environment or .env; otherwise try the free CDN.
import { mkdir, mkdtemp, readFile, rename, rm, writeFile } from 'node:fs/promises';
import { dirname, join } from 'node:path';

const version = '6.7.2';
const optionalFile = (path) => readFile(path, 'utf8').catch((error) => { if (error.code !== 'ENOENT') throw error; return undefined; });
async function request(url, options = {}, allowMissing = false) {
  const response = await fetch(url, { ...options, headers: { 'User-Agent': 'Mozilla/5.0', ...options.headers } });
  if (allowMissing && response.status === 404) return null;
  if (!response.ok) throw new Error(`FontAwesome request failed (${response.status})`);
  return response;
}

async function proIcon(name, style) {
  const token = process.env.FONTAWESOME_PRO_TOKEN || (await optionalFile('.env') ?? '').match(/^\s*FONTAWESOME_PRO_TOKEN=(.*)$/m)?.[1].trim();
  if (!token) return null;
  const { access_token } = await (await request('https://api.fontawesome.com/token', { method: 'POST', body: '', headers: { Authorization: `Bearer ${token}` } })).json();
  const response = await request('https://api.fontawesome.com', {
    method: 'POST', headers: { Authorization: `Bearer ${access_token}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: 'query($q:String!,$v:String!){search(version:$v,query:$q,first:15){id svgs{familyStyle{family style} html}}}', variables: { q: name, v: version } }),
  });
  const { data } = await response.json();
  return (data?.search ?? []).filter((icon) => icon.id === name).flatMap((icon) => icon.svgs)
    .find(({ familyStyle }) => familyStyle.family === (style === 'duotone' ? 'duotone' : 'classic') && familyStyle.style === (style === 'duotone' ? 'solid' : style))?.html ?? null;
}

async function main() {
  const [name, style] = process.argv.slice(2);
  if (!name || !style) throw new Error('usage: fa-add.mjs ICON STYLE');
  const target = `static/public/assets/svgs/fa-sprites/${style}.svg`;
  const content = await optionalFile(target) ?? '<svg xmlns="http://www.w3.org/2000/svg" style="display:none">\n</svg>';
  if (content.includes(`id="${name}"`)) { console.log(`icon '${name}' already in ${target}`); return; }
  let svg = await proIcon(name, style), source = 'Pro API';
  if (svg === null) {
    const response = await request(`https://use.fontawesome.com/releases/v${version}/svgs/${style}/${name}.svg`, {}, true);
    svg = response && await response.text();
    source = 'free CDN';
  }
  if (svg === null) throw new Error(`icon '${name}' not found in style '${style}' (Pro API + free CDN). Check the name/style at fontawesome.com; ensure FONTAWESOME_PRO_TOKEN is set for Pro icons.`);
  const viewBox = svg.match(/viewBox="([^"]*)"/)?.[1];
  if (viewBox === undefined) throw new Error('no viewBox found in downloaded SVG');
  const inner = svg.replace(/<svg[^>]*>|<\/svg>|<!--.*?-->/gs, '').trim();
  const symbol = `  <symbol id="${name}" viewBox="${viewBox}">\n    ${inner}\n  </symbol>`;
  await mkdir(dirname(target), { recursive: true });
  const temporary = await mkdtemp(join(dirname(target), '.fa-add-'));
  try {
    const file = join(temporary, 'sprite.svg');
    await writeFile(file, content.replace('</svg>', `${symbol}\n</svg>`), { mode: 0o600 });
    await rename(file, target);
  } finally { await rm(temporary, { recursive: true, force: true }); }
  console.log(`added '${name}' to ${target} (via ${source})`);
  console.log(`use: faSprite_ "${name}" "${style}" "w-4 h-4"`);
}

main().catch((error) => { console.error(error.message); process.exitCode = 1; });
