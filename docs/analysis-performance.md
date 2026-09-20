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
selected pair, with duplicate queries removed. At that checkpoint, whole-map analysis was a benchmark;
the demo still analyzed the selected bases and their nearby connections. Batching reduces
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


## Cached map-wide base areas

The demo now surveys every base's three nearest reachable anchors, adding both directions,
with one batch per origin. Filtering neighbors by the cached original terrain components avoids
searches between disconnected islands. Full routes are discarded after their candidate spans are
collected. At the initial map-wide checkpoint, geometric span consolidation preceded a single
cut partition; the serial-cut follow-up below adds a provisional topology check. The completed result
is cached, so changing A/B only rebuilds the highlight texture; it performs no analysis request.
The worker retains one result and copies labels on delivery (four bytes per walk cell per copy).

A local Chromium smoke run at 25% widening measured the following worker elapsed times. These
are single diagnostic runs including cooperative yields and JSON conversion, not benchmark
medians or latency guarantees. They exclude map loading, base discovery, and artwork rendering.
The workload differs from the earlier benchmark because disconnected neighbors are now excluded
and A/B selection no longer contributes an extra query.

| Map | Directed surveys | Distinct observations -> boundaries | Worker elapsed (ms) |
| --- | ---: | ---: | ---: |
| Python 1.3 | 40 | 14 -> 14 | 283 |
| Python 1.6 | 40 | 14 -> 14 | 301 |
| Hunters 2021 | 64 | 45 -> 40 | 464 |
| Revolver SE 2.0 | 44 | 51 -> 48 | 324 |
| Lemon 1.1 | 46 | 27 -> 26 | 452 |

Python's sampled natural areas remain 5706/6040/5495 walk cells; the sampled Hunters mains
remain 12028/13997. Full per-cell area membership for those three Python 1.3 naturals and
two Hunters mains matches the committed selected-pair implementation exactly. A separate Node/WASM
run on Primeval Isles found only two directed queries among its 46 island bases and finished in 34ms, with no candidate cuts. These checks establish
regression observations, not authoritative strategic area labels.

The browser check exercised cancellation during progress followed by a successful retry,
selection changes without worker requests, independent A/B surveys, obstacle-toggle cache
retention, threshold invalidation, and delayed stale replies after map replacement. All five
maps also rendered successfully. Cancellation is cooperative: a synchronous origin search or
partition must return before the worker can observe it; cancelled results are discarded before
cache replacement or delivery. There is still no hard wall-clock budget for an origin search.


Eight Node tests cover query planning, deterministic consolidation, boundary limits, cancellation,
worker cache invalidation, and transferable-buffer ownership. The Vite production build passes.
This checkpoint changes demo JavaScript and documentation only; Rust/WASM algorithms are unchanged.


## Serial-cut consolidation

A provisional partition now supplies connectivity for recognizing short chains of repeated cuts.
The new pass builds the component multigraph, finds bridges with an iterative traversal, and
measures candidate intermediate components in one label-raster scan. Only three/four-span chains
passing both topology and geometry guards collapse. If any spans are removed, one additional
partition produces the published labels and assessments. There are no per-candidate full-map floods.

On Python 1.3 and 1.6, the 14 proposed boundaries become 12. The retained bottom-natural boundary
is the middle horizontal span `[[1968,3388],[2336,3388]]`, width 368 pixels. The discarded oblique
span was only about six pixels narrower, and had incorrectly excluded another 1295 walk cells from
the natural. Its area increases from 4768 to 6063 cells; the other three naturals are unchanged.
Hunters goes from 40 boundaries to 38, expanding the top-right main by 465 walk cells. Revolver,
Lemon, and Primeval Isles retain their previous boundaries in this run. Those unchanged cases
remain useful checks that the pass is conservative, not evidence of complete entrance coverage.


Follow-up validation passed 16 Node tests and the production demo build. The five-map browser
regression still passes progress/cancellation, cached highlighting, threshold invalidation, and
stale-reply checks, and explicitly verifies the expanded Python 1.6 natural and retained middle
span. Geometry remains heuristic; a compact room with no supplied route anchor may resemble a
corridor, and two-cut chains deliberately retain their previous behavior.


## Late-turn entrance profiles

Python 1.3 and 1.6's right ramp exposed a missing outward mouth: the +96/+128px stations
both showed widening, but the +160px tangent turned toward the destination depot. The detector
now permits two-sample evidence in that specific case. Fully supported three-sample profiles
retain priority during suppression and the candidate cap.

An old/new Node/WASM comparison across Python 1.3, Python 1.6, Hunters 2021, Revolver SE 2.0,
Lemon 1.1, and Primeval Isles found one changed directed survey on each Python version and no
changed surveys on the other four maps. Base catalogs and surveyed routes were identical.
The added span is `[[3728,1912],[3808,1992]]`, about 113px wide. Each Python map now has
13 boundaries; the right natural excludes another 112 walk cells of ramp, changing from 5495
to 5383 cells. The bottom natural remains 6063 cells with its retained middle span.

Validation passed 502 workspace tests, strict all-target clippy, formatting, 16 Node tests,
both WASM builds, and the production demo build. The browser regression verified both ends
of the right ramp, the earlier bottom-natural fix, five-map rendering, cancellation, cache
reuse, and stale-reply handling. Synthetic regressions cover early bends, widening only after
a turn, a straight profile that re-narrows at +160px, missing endpoint support, evidence
metadata, and preference for fully supported candidates.


## Crossing conflicts and flat duplicate strips

Luna 2.1 revealed conflicting directional hypotheses at its top-left junction. A 568px horizontal
span crossed two narrower spans; applying all three manufactured small partition components.
The demo now resolves proper interior crossings before partitioning, retaining narrower observed
spans. A terrain-aware follow-up also merges close parallel cuts enclosing a flat, base-free
strip. It reuses the provisional raster scan and the same final repartition as serial consolidation.

The seven-map before/after check used the same WASM and base catalogs. Luna goes from 28 boundaries
to 24: one crossing conflict and three flat duplicates removed. Its top-left main grows from 16280
to 17025 walk cells; the middle-right expansion grows from 7669 to 8175. Python 1.3/1.6, Lemon,
and Primeval Isles retain identical boundaries and per-base area masks. Hunters goes from 38 to
30 boundaries and Revolver from 48 to 40. No previously separate known base anchors are combined
on any of the seven maps. These observations are heuristic regression checks, not authoritative
strategic region labels.

All 26 Node tests and the production demo build pass. A six-map browser run includes Luna,
retains both Python right-ramp markers and the bottom-natural fix, and passes cancellation,
cached selection, obstacle-toggle independence, threshold changes, and stale-reply checks.
This follow-up changes demo orchestration only; the Rust/WASM entrance predicate is unchanged.

A separate diagnostic replay matched all 49 Luna source observations' wall-contact endpoints.
None of their candidate stations lies on a ramp-flagged cell. Reraying at actual slope stations
gives roughly 90-102px spans versus the existing adjacent mouths' 226-351px spans, but those
slope stations fail the confined-approach predicate. That checkpoint did not retarget
those ramp markers; the structural ramp pass below addresses the missing evidence.


## Structural ramps and internal junctions

A separate Rust ramp detector now supplies two full wall-contact spans for each recognized
flagged/elevation component. The WASM snapshot caches this result. Map-wide orchestration retains
these structural spans, replaces qualifying wider ramp-mouth approximations, and removes bounded
flat internal-junction cuts while reserving their exterior ports. These passes reuse component
metadata and aggregate region bounds in one raster scan per pass; they do not run a full-map flood
per candidate. After the initial provisional partition, ramp-mouth replacement uses
at most two changed passes, each with a fresh partition. Serial/flat consolidation and junction
removal each need at most one further partition when changed.

On Luna 2.1, ramp flags cover only the middle of each slope. A bounded refinement uses a verified
same-elevation buildable landing and the first local opening to move seven ends outward to
192-249px wall sections. The top-left lower end remains a conservative 90px section: its next
opening would cross over 600px of unrelated room. All eight retained sections fully separate their
crossing edges in the final partition. The top-left natural keeps its vertical outer exit and
contains 4372 walk cells. The internal cut formerly labeled E17
(`[[2552,2712],[2768,2928]]`) remains absent with its three surrounding exits retained.

The southwest main's stray vertical cut `[[772,2880],[772,3408]]` is removed. Its ramp retains exactly
two structural sections, replacing the old generic copies, including the multipart lower span.
Nearly coincident upper sections create tiny three-component triangles; their generic copies are
removed using bounded geometry and conservative component-union checks. The main and natural now
contain 14083 and 5127 walk cells, respectively, with one known base anchor each.

At this checkpoint, boundary counts in the seven-map check were Python 1.3/1.6 16 each, Hunters 2021 29,
Revolver SE 2.0 38, Lemon 1.1 29, Primeval Isles 0, and Luna 2.1 25. No previously separate
known base groups merge. This checkpoint does not claim unchanged masks on the other maps.
Python's earlier bottom-natural 6063-cell result and right-natural 5383-cell result remain intact.
The Node/WASM whole-map pass measured 221ms on Luna in this run; the browser measured 305ms.
These single-run observations include consolidation and partitioning but exclude base discovery.

Validation passed 513 workspace tests, strict all-target clippy, formatting, 52 Node tests,
both WASM builds, and the production demo build. Browser checks cover six maps, all four
two-ended Luna ramps, southwest duplicate removal, the single retained top-left natural exit,
the removed internal junction cut, cancellation, cached selection, and stale replies. A 60-second
ASan fuzz run completed 5849 executions in 61 seconds without a finding, including deterministic
ramp output, adjacent elevations, distinct end geometry, walkable flagged-or-unbuildable positions,
in-bounds endpoints, and span-length invariants. These checks establish the reported examples and
structural invariants, not exhaustive coverage of custom ramps or game-accurate wall positions.


## Circuit Breakers: ramp shoulders and sealed interiors

Circuit Breakers 1.0 exposed initial ramp-end sections that already reached across expansion
floors before outward refinement ran. The side expansions had 633px lower sections, and one main
ramp had an 860px section. A bounded inward search now selects the first central section within
eight projection units that contracts by at least 64px and 25%, while reserving the minimum gap
between the two ends. It chooses centrality before applying the contraction test, preventing
narrow off-center nooks from winning. Tests cover all eight orientations, genuinely wide ramps,
and short ramps whose ends must not collapse onto the midpoint.

The side lower sections are now 181px, and the oversized main-ramp section is 102px. Their lower
base areas grow from 4542/4567 to 5305/5324 walk cells. The top and bottom expansions grow from
3581/5272 to 6326/6192 cells. The top-left natural's extra diagonal
`[[1152,160],[1536,544]]` is removed using the bounded two-port approach rule; its area grows from
3409 to 5670 cells. Its center-facing horizontal cut is retained: removing that one would join
the natural to the central terrain component.

A further graph pass removes seven ordinary cuts sealed between opposite ends of four ramps.
It retains both structural ends, rejects base anchors and additional exits, and requires ramp
cells and compatible elevations. Multipart cuts must be contained in their entirety; tests cover
one physical span that also crosses a separate pair of anchored regions. The pass shares one
region-statistics scan and adds at most one repartition, not one flood per candidate.

The final eight-map boundary counts are Python 1.3/1.6 15 each, Hunters 2021 29, Revolver SE 2.0 36,
Lemon 1.1 29, Primeval Isles 0, Luna 2.1 25, and Circuit Breakers 1.0 46 (down from 55).
No previously separate known base groups merge. Circuit Breakers' pre-existing shared central
component for its bottom naturals remains a limitation; these checks do not certify every base
boundary on the map. Luna's southwest correction and Python's 6063/5383-cell natural regressions
remain intact.

Validation passed 516 workspace tests, strict all-target clippy, formatting, 57 Node tests,
both WASM builds, and the production demo build. Browser checks cover Circuit Breakers' side,
top/bottom, and natural views plus the existing six-map and worker regressions. An ASan fuzz run
completed 6180 executions in 61 seconds without findings. Circuit Breakers' whole-map browser
analysis measured 370ms in this run, excluding base discovery; this is a single-run observation.


## Circuit Breakers: bridge junctions and the northeast mineral pocket

A follow-up removes three more ordinary cuts, reducing the map from 46 to 43 boundaries.
The two diagonal cuts between the northern bridges each isolated a flat, anchor-free pocket
with exactly three ports: a wide connection to the center, a narrow bridge to that same center,
and a separate entrance toward an anchored base. The junction pass now recognizes this shape,
removes the wide cut, and retains both other markers. It requires a 2:1 width ratio, bounded
geometry, a single elevation, and no ramp cells in the pocket. Shared center regions are allowed,
while reservations prevent one selected removal from consuming another's pocket or retained ports.
The narrow bridge markers remain useful physical sections even where the wider connection makes
them bypassable; the final partition reports that bypass.

The northeast diagonal `[[2560,544],[2944,160]]` crossed both a ramp approach and an empty leaf
behind the minerals. The broad-mouth locality exception now covers this strict multipart star:
the approach has only the mouth and matching ramp as exits, and each leaf has only the mouth.
Its corners must remain within 512px of either the mouth or ramp center. Removing the diagonal
expands that base area from 4306 to 6551 walk cells while retaining its sole known base anchor.

The other seven maps have identical boundary metadata and base-area masks to the preceding
checkpoint. No previously separate known base groups merge on any of the eight maps. Circuit
Breakers' center gains the two bridge pockets, totaling 3383 cells; its pre-existing shared
bottom-natural component remains unchanged in membership.

Validation for this JavaScript-only follow-up passed 63 Node tests, the production demo build,
and browser assertions for the three removed spans and the retained surrounding exits. Visual
inspection confirms the northeast pocket now belongs to its base. The browser whole-map analysis
measured 381ms in this single run, excluding base discovery. Rust and fuzz checks were not rerun
for this follow-up; the preceding checkpoint records their latest results.


## Laplace: natural splits, ramp duplicates, and buildability context

Laplace 1.0's northwest natural was split by `[[892,688],[892,1216]]`, a 528px ordinary
section wider than each of its three surrounding entrances. The junction selector now permits
one anchored component in a local union, retains every exterior port, and requires the removed
section to be at least as wide as those ports. Both components must have no ramp cells and share
a dominant elevation covering at least 99% of each: Laplace has only eight differently flagged
edge cells across 4493 cells. For this anchored case, bbox locality is measured within 512px of
the finite span; retained port centers still lie within 384px of its midpoint. The natural grows
from 2496 to 4493 walk cells with its single known base anchor and all three entrances intact.

Four generic ramp duplicates are removed while all sixteen structural ends remain. The northern
ramp interiors have additional leaves touched only by a multipart ramp port; they now participate
in the same anchor, elevation, and locality checks. The southern duplicates use a strict two-port
approach with bounded parallel overlap instead of relying solely on nearby endpoints. Tests reject
anchors, extra exits, narrow necks, poor overlap, and remote geometry.

Sections with no terrain-buildable cell within 64px of their midpoint now render muted gray.
This includes Laplace's long vertical sections across unbuildable ground. It is a spatial display
hint, not a placement or wallability test; it never removes a region boundary. The test measures
distance to cell rectangles and does not consult occupancy or elevation.

Laplace now has 35 boundaries, down from 40. Seven earlier maps retain identical boundary geometry,
partition metadata, and base-area masks (apart from the new buildability hint). Circuit Breakers
loses four additional redundant sections near its side ramps, reaching 39 boundaries; both
structural ends remain and the side-base masks are unchanged. Only its existing shared central
component grows. No previously separate known base groups merge in the nine-map comparison.

Validation passed 72 Node tests and the production demo build. Browser assertions and visual
inspection cover Laplace's merged natural, all retained ramp ends, the muted unbuildable sections,
and Circuit Breakers' side bases and retained exits. Browser whole-map analysis measured 259ms
for Laplace and 396ms for Circuit Breakers in these single runs, excluding base discovery.
No Rust changed in this follow-up; native and fuzz validation remains as recorded at the preceding
Rust checkpoint.


## Reusable Rust topology analyzer

The map-wide pipeline now lives in `broodmap_analysis::topology`. `TopologyJob` accepts an
immutable terrain grid plus caller base IDs and route anchors, and returns owned sections,
observations, the final partition, and base membership. `base(id)` and `entrances_for_base(id)`
query that result. Boundary assessments retain all incident area IDs, including bypassable
crossings, so a per-base query does not silently omit a section just because another route exists.
No placement or wallability guarantee is introduced.

The demo calls the incremental WASM job and retains only scheduling, cancellation, cache ownership,
and display hints. All consolidation rules share a Rust partition graph/statistics view. The
previous JavaScript policy is frozen under `broodmap-wasm/tests/topology-reference/` solely as a
comparison oracle; it is absent from the production bundle. This checkpoint preserves the existing
rules rather than introducing new map-specific selection cases.

The optional real-asset harness is:

```text
node broodmap-wasm/tests/topology-parity.mjs <asset-directory> <map>...
```

It compares endpoints, provenance, counts, assessments, base membership, display hints, and every
partition label, then repeats with reversed base input. All nine maps match exactly:

| Map | Retained boundaries |
| --- | ---: |
| Python 1.3 | 15 |
| Python 1.6 | 15 |
| Hunters 2021 | 29 |
| Revolver SE 2.0 | 36 |
| Lemon 1.1 | 29 |
| Primeval Isles | 0 |
| Luna 2.1 | 25 |
| Circuit Breakers 1.0 | 39 |
| Laplace 1.0 | 35 |

One Node/WASM pass measured 136-400ms for the Rust job and 84-448ms for the reference, excluding
base discovery. These single observations ran alongside other verification and establish no
speedup claim. Browser observations were 274ms for Laplace and 446ms for Circuit Breakers. A job
step is a cooperative scheduling point, not a hard time budget: construction labels original
terrain, and one later origin search or repartition can still visit the full grid.

Validation passes all 528 workspace tests, strict all-target clippy, formatting, 76 Node tests,
both WASM builds, and the production demo build. Real-browser checks cover cancellation during
origin work and after completion in transit, stale replies after map replacement, selection
cache reuse, threshold invalidation, obstacle-toggle independence, and six-map rendering.
Laplace and Circuit Breakers additionally assert retained exits and previously merged pockets;
Laplace's rendered natural areas were visually inspected.

At this extraction checkpoint the fuzz target exercised the full stepped pipeline on bounded
random grids and a coherent hallway-to-room fixture (the fixed fixture was removed below). It checks reordered/repeated-input determinism, progress completion,
repartition equality, base membership, invalid options, and nonempty entrance evidence on the
fixture. Assets and map inputs remain local, uncommitted dependencies.

ASan completed 2697 executions in 61 seconds before the final additive incidence field. With that
field, a second run replayed 2482 corpus inputs in 124 seconds with no findings; corpus startup
exceeded the requested 60-second budget, so that run did not reach new mutations. The real-grid
incidence tests independently cover separating, bypassed, unrelated, and zero-edge sections.


### Review follow-up: production selector coverage

Added 23 Rust selector tests: complete-link grouping and anchor projection, narrow/ramp crossing
precedence, serial anchor/ramp protection and multigraph cycles, ramp-mouth and sealed-interior
evidence gates, near-flat junction thresholds, bridge pockets, and proposal reservations. Most
selection fixtures build actual terrain partitions; focused geometry and graph tests isolate the
remaining predicates. They run in normal workspace CI without local game tables.

The five frozen-reference test suites now live next to their reference modules. `pnpm test` in
`broodmap-wasm` runs them together with the production demo tests. They remain oracle checks;
they are not counted as coverage of the Rust selectors.

Equal-cut junction proposals now explicitly rank by region identity after width/geometry, so
port reservations never depend on proposal insertion order. A test demonstrates the later-removal
difference that motivated this policy. All nine map comparisons still match the frozen reference,
including reversed input order. The 8192 raw-observation limit remains an intentional additional
bound on the Rust job.

Shared geometry normalization replaces duplicate selector helpers, imports name their actual
module, and private serial selectors use the finite positive widths produced by the pipeline.
The fixed hallway fixture is no longer repeated in fuzzing; deterministic entrance evidence stays
in core unit tests. The separate fuzz workspace now passes its formatting check.

Validation passes 551 workspace tests, 76 Node tests, strict all-target/all-feature clippy, both
WASM builds, and formatting for both workspaces. The fuzz target type-checks on nightly.

A fresh ASan mutation smoke run, starting from the three committed seeds, completed 2157
executions in 31 seconds without findings after the review fixes.
