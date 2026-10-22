import { createHash } from 'node:crypto';
import { realpath } from 'node:fs/promises';
import { join } from 'node:path';
import { isDeepStrictEqual } from 'node:util';
import { z } from 'zod';
import { readPrivateArtifact } from './artifacts.mjs';

const files = { video: 'explanation.mp4', diagram: 'diagram.svg', captions: 'captions.vtt', before: 'before.png', cause: 'cause.png', change: 'change.png', after: 'after.png' };
const sha256 = z.string().regex(/^[a-f0-9]{64}$/);
export const explanationSchema = z.looseObject({ projectId: z.uuid(), runId: z.uuid(), candidateId: z.uuid(), source: sha256, files: z.record(z.enum(Object.values(files)), sha256) });
const request = z.object({
  explanation: explanationSchema,
  asset: z.enum(Object.keys(files)),
}).strict();

export async function readExplanationMedia(input, usageDir) {
  const { explanation, asset } = request.parse(input);
  const directory = join(await realpath(usageDir), 'fixes', explanation.runId, 'checks', 'candidates', explanation.candidateId, 'explanations', explanation.source);
  const retained = JSON.parse(await readPrivateArtifact(directory, 'manifest.json', 65536));
  if (!isDeepStrictEqual(explanation, retained)) throw new Error('Media manifest changed');
  const bytes = await readPrivateArtifact(directory, files[asset], 32 * 1048576);
  if (createHash('sha256').update(bytes).digest('hex') !== explanation.files[files[asset]]) throw new Error('Media digest changed');
  return bytes;
}
