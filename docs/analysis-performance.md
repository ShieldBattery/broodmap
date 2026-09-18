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
