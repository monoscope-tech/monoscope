// Re-drive parked OTLP messages back into the live DLQ replay carousel.
//
// Context (2026-07-06): ~100k messages sat in otlp_deadletter-parking, all with
// error-reason="unsupported ce-type" and a BLANK ce-type header. Root cause was
// the old ceTypeFor guard (Pkg.Queue) blanking ce-type on the first retry-tier
// hop, turning every once-failed message into permanent decode-poison. The
// originals were valid OTLP payloads whose PG write had already SUCCEEDED
// (monoscope-pg-succeeded=true) and only the TF write failed
// (monoscope-write-failure=tf-failed). ceTypeFor is now fixed (commit 866541ae4)
// to recover ce-type from the original-topic header, so re-processing them
// decodes cleanly and — because the replay path targets only the failed store
// via monoscope-write-failure — retries just the TF write (no PG duplicates).
//
// Mechanism: snapshot the parking window [log-start, log-end) per partition
// (parking has no consumer group), consume it once, and re-produce each record
// to the otlp_deadletter BASE tier with:
//   - monoscope-attempt-count reset to "1"      (full retry ladder again; a
//     single transient TF hiccup retries instead of re-parking at attempt 5)
//   - monoscope-next-due-at dropped              (immediately due)
//   - every other header preserved verbatim      (original-topic drives ce-type
//     recovery; monoscope-write-failure keeps the write-target = TF-only)
//   - key + value bytes preserved verbatim
// The LIVE apitoolkit_eu_dlq consumer picks them up off otlp_deadletter and
// processes them — so NO consumer stop is required (this only produces, and
// trims a topic with no consumer). Re-produce uses acks=all; parking is trimmed
// only AFTER a confirmed flush, and even then the re-driven copy is durable in
// the otlp_deadletter carousel, so trimming can't lose data.
//
// Defaults to a dry run. Usage:
//
//	cd scripts/local/dlq-redrive
//	go run . -user p3superuser -pass "$KAFKA_PASSWORD"                       # dry run: window sizes
//	go run . -user p3superuser -pass "$KAFKA_PASSWORD" -apply -partition 5 -max 300   # canary (no trim)
//	go run . -user p3superuser -pass "$KAFKA_PASSWORD" -apply -trim          # full drain + trim parking
//
// Timestamp-range mode (2026-07-08 TF acked-write-loss backfill): re-drive a
// slice of the BASE DLQ itself — messages already consumed-and-committed whose
// TF writes were later lost (headers carry monoscope-write-failure=tf-failed +
// pg-succeeded=true, so replay retries only the TF leg). The window is
// snapshotted before producing, so src==dst cannot loop; -trim is refused in
// this mode (never DeleteRecords the live carousel).
//
//	go run . -user … -pass … -src otlp_deadletter \
//	  -from-ts 1783516200000 -to-ts 1783521600000              # dry run: window sizes
//	  … -apply -max 5000                                       # canary, then full with -apply
package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
	"sync/atomic"
	"time"

	"github.com/twmb/franz-go/pkg/kadm"
	"github.com/twmb/franz-go/pkg/kgo"
	"github.com/twmb/franz-go/pkg/sasl/scram"
)

func main() {
	brokers := flag.String("brokers", "redpanda-0.s.past3.tech:19092,redpanda-1.s.past3.tech:29092,redpanda-2.s.past3.tech:39092", "comma-separated bootstrap brokers")
	user := flag.String("user", "", "SASL username")
	pass := flag.String("pass", "", "SASL password")
	srcTopic := flag.String("src", "otlp_deadletter-parking", "parking topic to re-drive")
	dstTopic := flag.String("dst", "otlp_deadletter", "base DLQ tier to re-produce into")
	partition := flag.Int("partition", -1, "restrict to a single source partition (-1 = all)")
	maxN := flag.Int64("max", 0, "cap total messages re-driven (0 = all in window) — use for a canary")
	apply := flag.Bool("apply", false, "actually re-produce (default: dry run reports window sizes only)")
	trim := flag.Bool("trim", false, "after a confirmed flush, DeleteRecords the re-driven parking window (removes originals)")
	fromTs := flag.Int64("from-ts", 0, "epoch-ms start of a timestamp-range re-drive (overrides log-start)")
	toTs := flag.Int64("to-ts", 0, "epoch-ms exclusive end of a timestamp-range re-drive (overrides log-end)")
	stateFile := flag.String("state", "", "JSON file persisting per-partition progress; restarts resume past it")
	flag.Parse()

	if *user == "" || *pass == "" {
		log.Fatal("--user and --pass are required")
	}
	if (*fromTs == 0) != (*toTs == 0) {
		log.Fatal("-from-ts and -to-ts must be given together")
	}
	// Trim deletes source records; only ever safe on the consumerless parking
	// topic, and never for a range slice (the rest of the log is live data).
	if *trim && (*fromTs != 0 || *srcTopic != "otlp_deadletter-parking") {
		log.Fatal("-trim is only allowed for whole-window parking re-drives")
	}

	sasl := scram.Auth{User: *user, Pass: *pass}.AsSha256Mechanism()
	commonOpts := []kgo.Opt{
		kgo.SeedBrokers(strings.Split(*brokers, ",")...),
		kgo.SASL(sasl),
		kgo.ClientID("dlq-redrive"),
	}
	ctx := context.Background()

	adminCl, err := kgo.NewClient(commonOpts...)
	if err != nil {
		log.Fatalf("admin client: %v", err)
	}
	adm := kadm.NewClient(adminCl)
	defer adminCl.Close()

	starts, err := adm.ListStartOffsets(ctx, *srcTopic)
	if err != nil {
		log.Fatalf("list start offsets: %v", err)
	}
	leos, err := adm.ListEndOffsets(ctx, *srcTopic)
	if err != nil {
		log.Fatalf("list end offsets: %v", err)
	}
	// Timestamp-range mode: narrow [start, end) per partition to the first
	// offsets at/after each timestamp. A partition with no message at/after a
	// timestamp reports offset -1 — start falls back to log-start(no older
	// retained data), end to log-end.
	if *fromTs != 0 {
		fromOffs, err := adm.ListOffsetsAfterMilli(ctx, *fromTs, *srcTopic)
		if err != nil {
			log.Fatalf("list offsets at from-ts: %v", err)
		}
		toOffs, err := adm.ListOffsetsAfterMilli(ctx, *toTs, *srcTopic)
		if err != nil {
			log.Fatalf("list offsets at to-ts: %v", err)
		}
		starts.Each(func(o kadm.ListedOffset) {
			if f, ok := fromOffs.Lookup(*srcTopic, o.Partition); ok && f.Offset > o.Offset {
				starts[o.Topic][o.Partition] = f
			}
		})
		leos.Each(func(o kadm.ListedOffset) {
			if t, ok := toOffs.Lookup(*srcTopic, o.Partition); ok && t.Offset >= 0 && t.Offset < o.Offset {
				leos[o.Topic][o.Partition] = t
			}
		})
	}

	// Resume state: per-partition offset already re-driven (exclusive). WAN
	// stalls kill runs mid-window (watchdog below); restarting from the
	// window start re-produces everything, so persist progress and resume.
	resume := map[int32]int64{}
	if *stateFile != "" {
		if b, err := os.ReadFile(*stateFile); err == nil {
			if err := json.Unmarshal(b, &resume); err != nil {
				log.Fatalf("corrupt state file %s: %v (delete it to restart the window)", *stateFile, err)
			}
			fmt.Printf("resuming from %s: %v\n", *stateFile, resume)
		}
	}

	type pRange struct{ start, end int64 }
	ranges := map[int32]pRange{}
	var totalMsgs int64
	leos.Each(func(o kadm.ListedOffset) {
		if *partition >= 0 && o.Partition != int32(*partition) {
			return
		}
		start := int64(0)
		if e, ok := starts.Lookup(*srcTopic, o.Partition); ok {
			start = e.Offset
		}
		if r, ok := resume[o.Partition]; ok && r > start {
			start = r
		}
		if start >= o.Offset {
			return
		}
		ranges[o.Partition] = pRange{start: start, end: o.Offset}
		totalMsgs += o.Offset - start
	})
	if len(ranges) == 0 {
		fmt.Println("nothing to re-drive — parking window is empty")
		return
	}

	parts := make([]int32, 0, len(ranges))
	for p := range ranges {
		parts = append(parts, p)
	}
	sort.Slice(parts, func(i, j int) bool { return parts[i] < parts[j] })
	for _, p := range parts {
		r := ranges[p]
		fmt.Printf("partition %d: window [%d, %d)  msgs=%d\n", p, r.start, r.end, r.end-r.start)
	}
	fmt.Printf("TOTAL in window: %d messages  (re-produce %q -> %q)\n", totalMsgs, *srcTopic, *dstTopic)
	if *maxN > 0 {
		fmt.Printf("CAP: stop after %d messages (canary)\n", *maxN)
	}
	if !*apply {
		fmt.Println("DRY RUN — writing nothing. Re-run with -apply to re-drive.")
		return
	}

	prod, err := kgo.NewClient(append(commonOpts,
		kgo.ProducerLinger(20*time.Millisecond),
		kgo.ProducerBatchMaxBytes(64<<20),
		kgo.MaxBufferedBytes(256<<20),
		kgo.MaxBufferedRecords(4_000),
		kgo.ProducerBatchCompression(kgo.SnappyCompression()),
		kgo.RequiredAcks(kgo.AllISRAcks()),
	)...)
	if err != nil {
		log.Fatalf("producer: %v", err)
	}
	defer prod.Close()

	pOffsets := map[int32]kgo.Offset{}
	for p, r := range ranges {
		pOffsets[p] = kgo.NewOffset().At(r.start)
	}
	cons, err := kgo.NewClient(append(commonOpts,
		kgo.ConsumePartitions(map[string]map[int32]kgo.Offset{*srcTopic: pOffsets}),
		// Modest fetch sizes: the original 64-128MB values (tuned for a
		// datacenter-local run) wedged the first fetch when run over a WAN
		// (2026-07-08); 8MB fetches stream fine and the producer is the
		// bottleneck anyway.
		kgo.FetchMaxBytes(8<<20),
		kgo.FetchMaxPartitionBytes(4<<20),
		kgo.BrokerMaxReadBytes(32<<20),
	)...)
	if err != nil {
		log.Fatalf("consumer: %v", err)
	}
	defer cons.Close()

	// Re-driven per source partition — end offset actually consumed, so -trim
	// deletes exactly what we re-produced (never ahead of it).
	drivenTo := map[int32]int64{}
	// Seed with resumed progress so a checkpoint written this run doesn't
	// erase completed partitions' frontiers from the state file.
	for p, o := range resume {
		drivenTo[p] = o
	}
	var produced, produceFailed atomic.Int64
	var queued int64 // synchronous count for the -max cap (ack counters lag)
	remaining := map[int32]int64{}
	for p, r := range ranges {
		remaining[p] = r.end - r.start
	}

	start := time.Now()
	tick := time.NewTicker(5 * time.Second)
	defer tick.Stop()
	go func() {
		var lastProduced int64
		stalledSince := time.Now()
		for range tick.C {
			d := time.Since(start).Seconds()
			p := produced.Load()
			fmt.Printf("[%6.0fs] produced=%d/%d failed=%d rate=%.0f/s\n", d, p, totalMsgs, produceFailed.Load(), float64(p)/d)
			// Stall watchdog: a half-dead WAN connection can wedge the
			// producer indefinitely (2026-07-08: 10+ min at zero progress
			// while the cluster was healthy). Exit non-zero so a wrapper
			// loop restarts us — re-producing the overlap is safe (replay
			// is idempotent; TF dedup collapses).
			if p != lastProduced {
				lastProduced = p
				stalledSince = time.Now()
			} else if time.Since(stalledSince) > 3*time.Minute {
				log.Fatalf("STALLED: no produce progress for 3m (produced=%d) — exiting for wrapper restart", p)
			}
		}
	}()

	// Checkpoint: flush-barrier then persist drivenTo. After Flush returns,
	// every consumed record is ACKED, so drivenTo is a safe resume frontier
	// (resuming past an un-acked record would silently drop it from the
	// re-drive). Worst case on a crash: ≤ one checkpoint interval of
	// duplicates, which the idempotent replay absorbs.
	checkpoint := func() {
		if *stateFile == "" {
			return
		}
		if err := prod.Flush(ctx); err != nil {
			log.Printf("checkpoint flush failed (state not persisted): %v", err)
			return
		}
		b, _ := json.Marshal(drivenTo)
		tmp := *stateFile + ".tmp"
		if err := os.WriteFile(tmp, b, 0o644); err == nil {
			_ = os.Rename(tmp, *stateFile)
		}
	}
	lastCkpt := time.Now()

	done := false
	for !done {
		if time.Since(lastCkpt) > time.Minute {
			checkpoint()
			lastCkpt = time.Now()
		}
		fetches := cons.PollFetches(ctx)
		if errs := fetches.Errors(); len(errs) > 0 {
			for _, e := range errs {
				log.Printf("fetch err %s/%d: %v", e.Topic, e.Partition, e.Err)
			}
		}
		fetches.EachRecord(func(rec *kgo.Record) {
			if done {
				return
			}
			// HARD BOUND at the snapshotted window end. kgo keeps fetching a
			// partition after its window count is exhausted, and with
			// src==dst the fetch eventually reaches our own re-produced
			// copies — re-producing those is a self-amplifying loop
			// (2026-07-08: 555k excess produces before the kill). remaining
			// counting alone does NOT stop consumption; this guard does.
			if rec.Offset >= ranges[rec.Partition].end {
				if _, live := remaining[rec.Partition]; live {
					delete(remaining, rec.Partition)
					if len(remaining) == 0 {
						done = true
					}
				}
				return
			}
			// Rebuild headers: reset attempt-count, drop next-due-at, keep rest.
			hdrs := make([]kgo.RecordHeader, 0, len(rec.Headers)+1)
			hasAttempt := false
			for _, h := range rec.Headers {
				switch h.Key {
				case "monoscope-next-due-at":
					continue
				case "monoscope-attempt-count":
					hdrs = append(hdrs, kgo.RecordHeader{Key: h.Key, Value: []byte("1")})
					hasAttempt = true
				default:
					hdrs = append(hdrs, kgo.RecordHeader{Key: h.Key, Value: h.Value})
				}
			}
			if !hasAttempt {
				hdrs = append(hdrs, kgo.RecordHeader{Key: "monoscope-attempt-count", Value: []byte("1")})
			}
			out := &kgo.Record{Topic: *dstTopic, Key: rec.Key, Value: rec.Value, Headers: hdrs}
			prod.Produce(ctx, out, func(_ *kgo.Record, err error) {
				if err != nil {
					produceFailed.Add(1)
					log.Printf("produce failed (src %s/%d@%d): %v", rec.Topic, rec.Partition, rec.Offset, err)
				} else {
					produced.Add(1)
				}
			})
			if rec.Offset+1 > drivenTo[rec.Partition] {
				drivenTo[rec.Partition] = rec.Offset + 1
			}
			queued++
			remaining[rec.Partition]--
			if remaining[rec.Partition] <= 0 {
				delete(remaining, rec.Partition)
			}
			if (*maxN > 0 && queued >= *maxN) || len(remaining) == 0 {
				done = true
			}
		})
	}

	if err := prod.Flush(ctx); err != nil {
		log.Fatalf("flush: %v", err)
	}
	checkpoint()
	fmt.Printf("DONE re-drive: produced=%d failed=%d\n", produced.Load(), produceFailed.Load())
	if produceFailed.Load() > 0 {
		log.Fatalf("ABORT: %d produce failures — NOT trimming parking (originals preserved)", produceFailed.Load())
	}

	if *trim {
		offs := kadm.Offsets{}
		for p, upTo := range drivenTo {
			offs.AddOffset(*srcTopic, p, upTo, -1)
		}
		resp, err := adm.DeleteRecords(ctx, offs)
		if err != nil {
			log.Fatalf("DeleteRecords failed (re-drive already succeeded — safe to re-run with -trim only): %v", err)
		}
		resp.Each(func(d kadm.DeleteRecordsResponse) {
			if d.Err != nil {
				log.Printf("trim %s/%d failed: %v", d.Topic, d.Partition, d.Err)
			} else {
				fmt.Printf("trimmed %s/%d up to offset %d (new log-start=%d)\n", d.Topic, d.Partition, offs[*srcTopic][d.Partition].At, d.LowWatermark)
			}
		})
	} else {
		fmt.Println("(-trim not set: parking originals left in place)")
	}
}
