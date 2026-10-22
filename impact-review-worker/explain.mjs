import { createHash } from 'node:crypto';
import { mkdir, readFile, realpath, mkdtemp, writeFile, rename, rm, open } from 'node:fs/promises';
import { join } from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import lockfile from 'proper-lockfile';
import { z } from 'zod';
import { readPrivateArtifact, verifyFixArtifacts } from './artifacts.mjs';

const exec = promisify(execFile);
const hash = (bytes) => createHash('sha256').update(bytes).digest('hex');
const text = (max) => z.string().min(1).max(max).refine((value) => value.trim().length > 0 && !/[\x00-\x08\x0b-\x1f\x7f]/.test(value));
export const presentationSchema = z.object({ cause: text(240), change: text(180), before: text(360), after: z.union([text(360), z.literal('')]) }).strict();

export async function renderExplanation(request, draft, { usageDir, cacheDir, signal }) {
  const result = await verifyFixArtifacts(request, { usageDir, cacheDir, signal });
  const checks = join(await realpath(usageDir), 'fixes', result.runId, 'checks');
  const execution = join(checks, result.checkId), candidate = join(checks, 'candidates', result.candidateId);
  const report = JSON.parse(await readPrivateArtifact(execution, 'report.json', 65536));
  const presentation = presentationSchema.parse(draft ?? { cause: result.rootCause, change: result.summary, before: report.failureMessage, after: '' });
  if (!result.rootCause.includes(presentation.cause) || !result.summary.includes(presentation.change)) throw new Error('Explanation must use the accepted cause and change');
  for (const [side, receipt] of [['base', request.baseline], ['head', request.candidate]]) {
    const bytes = await readPrivateArtifact(execution, `${side}.log`, 1048576);
    const excerpt = presentation[side === 'base' ? 'before' : 'after'];
    if (hash(bytes) !== receipt.logSha256 || !bytes.toString('utf8').includes(excerpt)) throw new Error('Explanation excerpt does not match recorded output');
  }
  if (!presentation.before.includes(report.failureMessage)) throw new Error('Before excerpt must show the reproduced assertion');
  const rendererSha256 = hash(await readFile(new URL(import.meta.url)));
  const source = hash(JSON.stringify({ version: 1, rendererSha256, result, executions: [request.baseline.id, request.candidate.id, request.baseline.imageBuildId], presentation }));
  const parent = join(candidate, 'explanations');
  await mkdir(parent, { recursive: true, mode: 0o700 });
  if (await realpath(parent) !== parent) throw new Error('Invalid explanation directory');
  const directory = join(parent, source);
  const release = await lockfile.lock(candidate, { stale: 120000, update: 10000, retries: { retries: 30, minTimeout: 100, maxTimeout: 1000 } });
  let temporary;
  const command = async (program, args, cwd) => exec(program, args, { cwd, signal, timeout: 120000, maxBuffer: 1048576, env: { PATH: process.env.PATH, LC_ALL: 'C' } });
  try {
    // A cache hit is usable only if every retained media file still matches.
    try {
      const manifest = JSON.parse(await readPrivateArtifact(directory, 'manifest.json', 65536));
      for (const [key, value] of Object.entries({ version: 1, source, rendererSha256, projectId: result.projectId, runId: result.runId, issueId: result.issueId, candidateId: result.candidateId, head: result.head, checkId: result.checkId, regressionSha256: result.regressionSha256, durationMs: 36000, width: 1280, height: 720, resourceCostStatus: 'unpriced' })) if (manifest[key] !== value) throw new Error('Stale explanation');
      for (const [name, expected] of Object.entries(manifest.files)) {
        if (!['explanation.mp4', 'diagram.svg', 'captions.vtt', 'before.png', 'cause.png', 'change.png', 'after.png'].includes(name)) throw new Error('Invalid media file');
        if (hash(await readPrivateArtifact(directory, name, 32 * 1048576)) !== expected) throw new Error('Changed media');
      }
      if (Object.keys(manifest.files).length !== 7) throw new Error('Incomplete explanation');
      signal?.throwIfAborted();
      return { directory, manifest, cached: true };
    } catch (error) { signal?.throwIfAborted(); if (error?.code === 'EACCES') throw error; }
    temporary = await mkdtemp(join(parent, '.render-'));
    const started = performance.now();
    const wrap = (value, width = 60) => value.split('\n').flatMap((line) => line.match(new RegExp(`.{1,${width}}(?:\\s|$)|.{1,${width}}`, 'gu')) ?? ['']).map((line) => line.trimEnd()).join('\n');
    // DOT labels are quoted data; no HTML labels, paths, or commands come from the model.
    const label = (value) => JSON.stringify(value);
    const graph = (body) => `digraph G { graph [bgcolor="#0f172a", pad="0.65", nodesep="0.5", ranksep="0.5"]; node [fontname="Helvetica", fontsize=26, fontcolor="#f8fafc", color="#334155", style="rounded,filled", fillcolor="#172338", margin="0.3,0.22", shape=box]; edge [color="#94a3b8", arrowsize=0.9]; ${body} }`;
    const diagram = graph(`rankdir=LR; base [label=${label('Base fails\n' + result.base.slice(0, 10))}, color="#fb7185"]; cause [label=${label('Inferred cause\n' + wrap(presentation.cause, 30))}]; change [label=${label('Change\n' + wrap(presentation.change, 30))}]; fixed [label=${label('Same test passes\n' + result.head.slice(0, 10))}, color="#34d399"]; base -> cause -> change -> fixed;`);
    await writeFile(join(temporary, 'diagram.dot'), diagram, { mode: 0o600 });
    await command('dot', ['-Tsvg', 'diagram.dot', '-o', 'diagram.svg'], temporary);
    const scenes = [
      { name: 'before', title: '01  /  REPRODUCE', heading: 'The regression fails on base', detail: presentation.before, note: `Recorded output · exit ${request.baseline.exitCode} · ${result.base.slice(0, 10)}`, color: '#fb7185' },
      { name: 'cause', title: '02  /  ROOT CAUSE', heading: 'Why the behavior failed', detail: presentation.cause, note: 'Agent explanation · inference from the investigated code', color: '#a5b4fc' },
      { name: 'change', title: '03  /  THE FIX', heading: 'The candidate keeps the regression', detail: presentation.change, note: 'Same test · same image · fresh verifier VMs', color: '#a5b4fc' },
      { name: 'after', title: '04  /  VERIFY', heading: 'The unchanged test passes on candidate', detail: presentation.after || 'Verified exit code: 0.', note: `${presentation.after ? 'Recorded output' : 'Recorded exit status'} · exit 0 · ${result.head.slice(0, 10)}\nCounterfactual: the same test still fails on base`, color: '#34d399' },
    ];
    for (const scene of scenes) {
      if (scene.name === 'cause') { await command('dot', ['-Tpng', 'diagram.dot', '-o', 'cause.png'], temporary); continue; }
      const content = graph(`a [label=${label(scene.title)}, shape=plain, fontsize=20, fontcolor=${label(scene.color)}]; b [label=${label(scene.heading)}, shape=plain, fontsize=32]; c [label=${label(wrap(scene.detail))}, fontname=${label(['before', 'after'].includes(scene.name) ? 'Courier' : 'Helvetica')}, color=${label(scene.color)}]; d [label=${label(scene.note)}, shape=plain, fontsize=18, fontcolor="#cbd5e1"]; e [label=${label('Monoscope · retained verification evidence · ' + result.checkId.slice(0, 8))}, shape=plain, fontsize=14, fontcolor="#94a3b8"]; a -> b -> c -> d -> e [style=invis];`);
      await writeFile(join(temporary, `${scene.name}.dot`), content, { mode: 0o600 });
      await command('dot', ['-Tpng', `${scene.name}.dot`, '-o', `${scene.name}.png`], temporary);
    }
    const captions = 'WEBVTT\n\n' + scenes.map((scene, index) => {
      const stamp = (seconds) => `00:00:${String(seconds).padStart(2, '0')}.000`;
      return `${stamp(index * 9)} --> ${stamp((index + 1) * 9)}\n${scene.heading}\n${scene.detail.replace(/\n\s*\n/g, '\n').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')}\n`;
    }).join('\n');
    await writeFile(join(temporary, 'captions.vtt'), captions, { mode: 0o600 });
    const inputs = scenes.flatMap((scene) => ['-loop', '1', '-t', '9', '-i', `${scene.name}.png`]);
    const filters = scenes.map((_, index) => `[${index}:v]scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2:color=0x0f172a,setsar=1,fps=24[v${index}]`).join(';') + ';[v0][v1][v2][v3]concat=n=4:v=1:a=0[out]';
    await command('ffmpeg', ['-hide_banner', '-loglevel', 'error', ...inputs, '-filter_complex', filters, '-map', '[out]', '-an', '-c:v', 'libx264', '-preset', 'veryfast', '-crf', '24', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', '-t', '36', 'explanation.mp4'], temporary);
    const probe = JSON.parse((await command('ffprobe', ['-v', 'error', '-show_streams', '-show_format', '-of', 'json', 'explanation.mp4'], temporary)).stdout);
    if (probe.streams.length !== 1 || probe.streams[0].codec_name !== 'h264' || probe.streams[0].width !== 1280 || probe.streams[0].height !== 720 || Number(probe.format.duration) !== 36) throw new Error('Invalid explanation encoding');
    const ffmpeg = (await command('ffmpeg', ['-version'], temporary)).stdout.split('\n')[0];
    const graphviz = (await command('dot', ['-V'], temporary)).stderr.trim();
    const files = {}; let mediaBytes = 0, videoBytes = 0;
    for (const name of ['explanation.mp4', 'diagram.svg', 'captions.vtt', ...scenes.map((scene) => `${scene.name}.png`)]) {
      const bytes = await readPrivateArtifact(temporary, name, 32 * 1048576);
      files[name] = hash(bytes); mediaBytes += bytes.length; if (name === 'explanation.mp4') videoBytes = bytes.length;
    }
    const manifest = { version: 1, source, rendererSha256, projectId: result.projectId, runId: result.runId, issueId: result.issueId, candidateId: result.candidateId, head: result.head, checkId: result.checkId, regressionSha256: result.regressionSha256, durationMs: 36000, width: 1280, height: 720, renderMs: Math.ceil(performance.now() - started), mediaBytes, videoBytes, resourceCostStatus: 'unpriced', tools: { ffmpeg, graphviz }, files };
    for (const name of ['diagram', 'before', 'change', 'after']) await rm(join(temporary, `${name}.dot`));
    await writeFile(join(temporary, 'manifest.json'), JSON.stringify(manifest) + '\n', { mode: 0o600 });
    for (const name of ['manifest.json', ...Object.keys(files)]) { const file = await open(join(temporary, name), 'r'); try { await file.sync(); } finally { await file.close(); } }
    const folder = await open(temporary, 'r'); try { await folder.sync(); } finally { await folder.close(); }
    await rm(directory, { recursive: true, force: true });
    await rename(temporary, directory); temporary = undefined;
    const folderParent = await open(parent, 'r'); try { await folderParent.sync(); } finally { await folderParent.close(); }
    signal?.throwIfAborted();
    return { directory, manifest, cached: false };
  } finally { try { if (temporary) await rm(temporary, { recursive: true, force: true }); } finally { await release(); } }
}
