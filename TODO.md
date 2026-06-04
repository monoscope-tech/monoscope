# TODO

## Ingestion pipeline

- **Decoder/writer pipelining (revisit later).** Ship per-partition fanout first, add a histogram for decode-time vs write-time, then revisit. If decode is <20% of cycle, skip this entirely. If it's 30%+ and fanout isn't enough, the pipeline is justified — and at that point, make the queue depth = 1 (double-buffer, not deep pipeline) to keep the in-flight set small and the commit logic simple.
