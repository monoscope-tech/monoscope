// LF-only JSONL records. readline also splits on U+2028/U+2029, which aeson emits raw inside strings.
export async function* jsonLines(stream, limitBytes = 16 * 1024 * 1024) {
  let chunks = [], size = 0;
  for await (let chunk of stream) {
    for (let end = chunk.indexOf(10); end >= 0; end = chunk.indexOf(10)) {
      if (size + end > limitBytes) throw new Error('Protocol record exceeds its size limit');
      chunks.push(chunk.subarray(0, end));
      yield Buffer.concat(chunks).toString('utf8').replace(/\r$/, '');
      chunks = []; size = 0; chunk = chunk.subarray(end + 1);
    }
    chunks.push(chunk); size += chunk.length;
    if (size > limitBytes) throw new Error('Protocol record exceeds its size limit');
  }
  if (size) yield Buffer.concat(chunks).toString('utf8');
}
