// LF-only JSONL records. readline also splits on U+2028/U+2029, which aeson emits raw inside strings.
export async function* jsonLines(stream) {
  let pending = Buffer.alloc(0);
  for await (const chunk of stream) {
    pending = Buffer.concat([pending, chunk]);
    for (let end = pending.indexOf(10); end >= 0; end = pending.indexOf(10)) {
      yield pending.subarray(0, end).toString('utf8').replace(/\r$/, '');
      pending = pending.subarray(end + 1);
    }
  }
  if (pending.length) yield pending.toString('utf8');
}
