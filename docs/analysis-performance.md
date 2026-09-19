# Base discovery profiling checkpoint

Measured on 2026-09-17 (local date), Windows, Intel Core i7-14700KF, Rust 1.98.1
release builds and headless Chromium. Baseline: `f945a9d`. These are local latency
observations, not performance guarantees or CI thresholds.

The main cost is repeated resource-distance searches. A smaller scoring-queue entry
improved native discovery by about 10-17% in controlled comparisons, with identical
results. The browser measurements did not establish a consistent improvement.
Further optimization can wait for another feature checkpoint; unusually many distinct
clearance sets are the important scaling case to retain.

## Reproducing native measurements

From the repository root, with locally exported game tables:

```sh
cargo run --release -p broodmap-analysis --example profile_bases -- broodmap-wasm/examples/assets 5 "path/to/map.scx" "path/to/another.scm" --stress --memory
```

The example normalizes stock resources, starts, and static obstacles like the WASM
adapter. That normalization is currently duplicated: changes to the stock input policy must
update both the example and `broodmap-wasm/src/analysis/bases.rs` until a shared adapter is
extracted. Parsing, asset loading, terrain construction, and input normalization happen
before timing. Each map receives one warmup followed by the requested number of
uncached discoveries; every output must equal the warmup output. TSV contains full
input paths, counts, min/median/max times, and optional allocation statistics. Errors
produce rows, the remaining cases still run, and the process exits nonzero if any fail.
No game assets or maps are bundled with the benchmark.

`--memory` measures an additional discovery with allocation tracking enabled. It
reports requested heap bytes above the pre-call baseline, excluding preloaded inputs,
allocator overhead, stacks, and process memory. The returned catalog is dropped before
the net-byte reading. Counting is disabled during timed iterations, though the allocator
wrapper still checks its enabled flag. `--stress` adds a fully walkable 256x256 map with
four resource patches and 100 small destructible objects near the candidate window;
this deliberately creates many distinct removal sets. It uses one warmup and one timed
iteration regardless of the requested repetition count, plus a memory pass if enabled.

## Results

Native times below are medians of five calls before/after the queue change. Browser
times are worker-reported medians of three calls, rounded to milliseconds, excluding
map loading, analysis construction, rendering, and UI round-trip overhead. Each browser
sample used a fresh page/worker; a separate discarded sample warmed loading caches,
not that worker's JIT. The worker timer includes JSON serialization/parsing. Maps were
measured serially, without concurrent benchmark or build jobs.

| Map | Resources / obstacles | Bases | Native before -> after (ms) | Browser before -> after (ms) | Native peak after (MiB) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Lemon 1.1 | 162 / 4 | 13 | 108.5 -> 91.9 | 191 -> 184 | 11.0 |
| Python 1.3 | 122 / 0 | 14 | 94.1 -> 82.6 | 171 -> 166 | 11.1 |
| Hunters 2021 | 160 / 0 | 17 | 125.2 -> 106.3 | 232 -> 239 | 9.3 |
| Horizon Lunar Colony 1.1 | 138 / 24 | 15 | 113.5 -> 98.0 | 200 -> 207 | 11.0 |
| Primeval Isles | 370 / 0 | 46 | 223.5 -> 198.3 | 384 -> 397 | 35.0 |
| Medusa 2.2 iCCup | 120 / 30 | 12 | 73.7 -> 65.8 | 132 -> 137 | 10.9 |

All six cases had zero unplaced clusters. Counts are observations, not annotations of
correct base locations. Primeval Isles is 256x256; the other five are 128x128. All native
memory passes returned to zero incremental requested bytes after dropping the result.

Two additional alternating native comparisons (compact/baseline/baseline/compact,
five samples per real map per batch) supported the improvement independently of the
single before/after run. Those comparisons used identically instrumented isolated
copies and checked complete `BaseDiscovery` values across variants. Browser changes
were small and mixed (about -4% to +4%); the small sample does not support a browser
speedup claim. Same-options browser cache hits on Lemon and Primeval rounded to 0 ms
inside the worker; UI round trips still cost roughly 8-13 ms.

The synthetic case produced **121 clearance groups / 484 resource searches**. Before
the change, two instrumented samples took 1.176-1.177 s; compact-queue samples took
0.996-1.030 s. The final uninstrumented example took 1.044 s with a 42.5 MiB incremental
heap peak. Groups are processed sequentially, so their cost accumulates primarily in
time rather than retaining a full grid for every group simultaneously.

## Where the time goes

An ignored source copy with phase timers measured the baseline. Approximate shares of
the accounted phase time on the six real maps:

- Resource-distance Dijkstra floods: **65-84%**.
- Per-group grid cloning/rasterization: **3-14%**.
- Terrain-only clustering: **7-10%**.
- Initial grids, candidate generation/evaluation, and final selection: the remainder.

The synthetic case spent about 68% in resource searches and 31% constructing group
grids. Thus a future optimization should consider repeated floods as well as grid copies.
Normal fixtures had only 12-46 groups total, generally one per qualifying cluster.
These source-level timers are diagnostic; their totals include instrumentation overhead
and omit small gaps between spans, so use the standalone example for latency baselines.

The applied optimization changes `ScoringScratch` heap entries from
`(cost, y, x, index)` to `(cost, index)`. For valid cells, `index = y * width + x` has the
same order as `(y, x)`, preserving every tie. Entry size falls from 24 to 16 bytes on
64-bit native and from 16 to 8 on wasm32. Overall memory savings are small because grid
and scratch arrays dominate. No search radius, score, clearing policy, or base selection
rule changed.

A separate Node run of the web WASM module measured linear-memory capacity (before the
queue change). Lemon grew from 6.44 MiB after analysis construction to 13.81 MiB after
discovery. Primeval grew from 21.88 to 42.19 MiB. They ran sequentially in one WASM
instance; these are allocator capacity/high-water observations, **not live heap usage or
browser process memory**. Capacity did not shrink after freeing the analysis objects.

## Installed maps and follow-up decisions

Inventorying `C:/Program Files (x86)/StarCraft/Maps` found 323 parseable maps, including
11 at 256x256. Primeval Isles has the largest supported resource count, 370. **Crystallis
has 890 resources on a 64x64 map**, exceeding the solver's current 512-resource limit;
the profiler confirms the explicit rejection. The limit is not a property of valid
StarCraft maps. This checkpoint leaves it unchanged; increasing it should be paired
with measurements of dense fields and an explicit work budget.

Keep the synthetic case when changing clearance handling. A work limit should report
that analysis was incomplete rather than silently discarding clearance groups. Reusing
grids/searches across identical sets, restricting work to a local region, or stopping
searches once all required candidate perimeters are settled are possible next experiments.
None is needed to continue ordinary-map feature development based on this baseline.

Validation: 51 focused Rust tests, strict workspace Clippy, formatting, complete native
output comparisons, rebuilt web/Node WASM packages, unchanged catalogs and route checks
on five real maps, six-map browser checks, and 51,722 terrain fuzz executions without a
finding. Map inputs, raw measurements, and the temporary instrumented copy stay in the
ignored `target/` directory; the reusable native example is the maintained artifact.


## Entrance surveys and experimental base areas

The entrance/area prototype now batches directed surveys by origin. A single Dijkstra
search settles all requested destinations for that origin, preserving the original
neighbor order, heap ties, and each directional route. The immutable terrain snapshot
also retains its clearance field and the ordinary components used to measure an area's
original size. Search trees are released after each origin; partitions with proposed
cuts are still computed per request.

A side-by-side comparison used the saved pre-optimization release Node/WASM build and
the rebuilt release module, with identical maps and demo query selection. Times below
are medians of seven interleaved before/after calls after correctness comparisons warmed
both implementations. They include surveys, JSON conversion, and original/cut area
partitions; they exclude map loading, base discovery, rendering, and worker round trips.
No build or other benchmark ran concurrently. These are local Node measurements, not
browser latency guarantees.

| Map | Selected pair before -> after (ms) | Whole-map before -> after (ms) | Whole-map directed queries |
| --- | ---: | ---: | ---: |
| Python 1.3 | 241.9 -> 106.0 | 949.7 -> 401.5 | 50 |
| Hunters 2021 | 295.5 -> 186.0 | 913.1 -> 364.2 | 66 |

Both selected-pair workloads contain 14 directed surveys, including nearby bases.
Whole-map workloads apply the same three-nearest-base policy to every base, plus the
selected pair, with duplicate queries removed. Whole-map analysis is a benchmark here;
the demo still analyzes the selected bases and their nearby connections. Batching reduces
whole-map time by about 58-60%, but it does not make its cost equal to a selected pair.

The first selected-pair call on a fresh analysis snapshot took median 124.7ms on Python
and 208.8ms on Hunters (three calls each, after module warmup). This includes preparing
the clearance and original components; map loading and base discovery remain excluded.
The retained fields use six bytes per walk cell for clearance and component labels,
about 1.5 MiB on a 128x128-build-tile map, plus component metadata and allocation overhead.
The underlying terrain grid is shared rather than copied into the prepared survey.

Complete old/new results matched on Python 1.3, Python 1.6, Hunters 2021, Revolver SE 2.0,
and Lemon 1.1: route points, distances, candidates, boundary metadata, and all area labels.
Selected-pair comparisons covered widening thresholds 15%, 25%, and 40%; whole-map
comparisons additionally checked reversed query order and obstacle-mode independence.
The browser regression covered those five maps, repeated requests, controls, ordinary
routing, and stale worker replies after map replacement.

Core regression tests compare batching against the original single-query algorithm on
64 generated grids, disconnected endpoints, duplicate/reversed queries, and a corridor
requiring expansion through an already settled destination. Batch output is limited to
256 queries and 2,097,152 total route points, including duplicates. The WASM JSON input
is additionally limited to 32 KiB. Requests exceeding a limit fail explicitly; they do not
silently omit surveys or return partial results. Raw comparisons and the saved baseline
remain in ignored `target/` files.

Validation also passed all 493 workspace tests, three demo orchestration tests, strict
workspace Clippy, formatting, and a Rust 1.95 all-targets check. A 26-second terrain fuzz
smoke completed 2,709 executions with no finding, including sampled batch-versus-single
query equivalence and reversed query ordering.


## Review follow-up: diagonal boundary sweeps

The original partitioner inspected every walk cell in each span's expanded bounding box.
A diagonal across a 1024x1024 grid therefore caused a full-map candidate scan per span.
The replacement enumerates a conservative strip along the span's major axis, visiting
at most seven cells per column or row, and applies the unchanged exact crossing predicate.
The final component flood still runs once over the grid. A deterministic regression
bounds 256 long diagonal spans to at most `256 * 1024 * 7` candidate cells and checks
that a span never enumerates the same candidate twice.

A native release diagnostic used an open 1024x1024 grid, with spans from `(0, offset)`
to `(8192, 8192 - offset)` for offsets 0 through 255, reversing every other span. One
baseline run took 5062ms; optimized runs took 234ms and 97ms. These are a few diagnostic
samples rather than a latency distribution. All reported 6735 components, with 4091
removed edges for the first span and 4030 for the last. One exact diagonal took 49ms
before and 51/36ms after; the remaining full-map component flood dominates that case.
Correctness is established separately by the independent finite-segment oracle and
complete old/new real-map output comparisons, not by these aggregate benchmark counts.

The entrance cleanup also omits four unused rays per eligible route cell: widths at
+32/+64 pixels never fed a filter. Both positions still participate in tangent checks,
preserving bend rejection. Region indices remain `usize`: narrowing them to `u32`
would save memory on 64-bit native builds, but they are already 32-bit on wasm32.

The 256-query entrance batch cap and aggregate route-point limit do not impose a CPU
budget. Every distinct origin may still require a full-grid search, so callers should
group queries by origin and yield between smaller batches when responsiveness matters.
This behavior is now stated beside the limits in both core and WASM API documentation;
no new origin cap or cancellation mechanism was introduced in this checkpoint.

Follow-up validation passed 496 workspace tests, three Node orchestration tests, strict
workspace Clippy, formatting, Rust 1.95 compatibility, rebuilt web/Node WASM and Vite,
and the five-map browser regression. All five complete pre-change WASM comparisons
still matched, including candidate geometry and every partition label. A 31-second
terrain fuzz smoke completed 3,839 executions without a finding.
