# Terrain analysis experiment

This checkpoint adds resolved terrain, static map obstacles, square clearance fields, terrain regions,
resource-base candidates, and point/base route comparisons. It is a foundation for base territory
and wall checking, not an engine-compatible movement solver or a complete building-placement checker.

## Ownership and inputs

- `broodmap` still parses CHK/MPQ, with its existing permissive semantics.
- `broodmap-formats` still parses CV5/VF4 and other game tables.
- `broodmap-analysis` owns the spatial grid and route queries. It depends on parsing crates,
  never rendering, filesystem, Bevy, or a particular game runtime.
- `broodmap-wasm` exposes an independent `TerrainAnalysis` snapshot; the example's worker
  owns WASM lifetimes. The browser draws overlays and paths in map coordinates.
- `AssetRequest::Vf4` reuses the existing CASC/directory/CDN transport. Rendering does not
  request it; `fetch-assets --analysis-only` exports CV5/VF4 plus units.dat without artwork.

`TerrainGrid::from_terrain` accepts CHK terrain and parsed CV5/VF4. Callers with resolved or
modded terrain can instead use `TerrainGrid::from_cells`. The current normalized interface
requires no artwork or unit definitions. `TerrainGrid::with_obstacles` accepts caller-supplied
half-open pixel rectangles, independent of unit IDs or game runtime; recompute from the original
terrain with a subset of rectangles to model removal without losing overlapping blockers.
The separate `melee_obstacles` adapter uses UNIT/THG2 and caller-supplied units.dat for initial
neutral resources/buildings. This is a melee approximation, not UMS or trigger simulation.

## Static obstacle policy

The melee adapter keeps neutral/unowned grounded buildings and resource containers using
units.dat flags and collision bounds, not their placement footprint. Player-owned non-resource placements,
start markers, hallucinated/lifted objects, intrinsic flyers, and decorative THG2
sprites are excluded. Grounded add-ons remain blockers. Disabled THG2 unit sprites remain candidates: hidden artwork does not
necessarily mean an absent blocker. Door state and start-HQ clearing are not simulated.
Following OpenBW's melee loader, UNIT resources survive regardless of owner, other UNIT
objects require owner 11 (or absent owner, retained conservatively), and THG2 requires owner 11.
Special door ownership overrides are not modeled. IN_TRANSIT exclusion remains an analysis
policy: the inspected OpenBW loader does not implement that state.
For UMS, live state, or other rules, supply the applicable rectangles directly.

Raw units.dat extents are inclusive: the adapter adds one to right/bottom to produce
half-open rectangles, matching neobrood gen_rules normalization. Zero extents cover one pixel.
The adapter skips definitions with any negative extent as unsupported. It does not claim
conservative coverage for such malformed/modded definitions; callers can normalize their own
collision shapes and pass explicit pixel rectangles instead.
Rectangle coordinates are signed, half-open logical pixels; out-of-map portions are clipped.
Empty/inverted rectangles do nothing. The terrain grid remains unchanged: applying blockers
produces a separate snapshot, preserving all terrain attributes except effective walkability.
The WASM adapter requires a complete standard-size units.dat, while the core rectangle API
also supports callers whose game definitions have a different format.

## Coordinate and terrain contract

A map tile is 32 logical pixels, containing sixteen 8-pixel walk cells. `WalkPosition`
is an integer walk-cell index, not a pixel coordinate. A cell's center is `(8*x+4, 8*y+4)`.
Rows run left to right, top to bottom. Image scaling never changes these coordinates.

The CHK adapter resolves MTXM group/index through CV5 and then VF4. The VF4/CV5
walkability aggregation has been checked against OpenBW's `set_mega_tile_flags` and
`is_walkable`: more than 12 walkable minitiles, or CV5 creep, makes the whole tile walkable;
otherwise each cell uses its VF4 walkability bit. Elevation is the raw minitile height
(high takes precedence over mid), and ramps use the VF4 ramp flag.

The adapter additionally honors MTXM bit 15 through broodmap's `TileId::has_creep`.
This was checked directly in the labeled `12310g.exe` game binary using Binary Ninja:
`read_chk_mtxm_section` (`0x140359410`) feeds the map tiles through
`convert_map_tiles_to_megatiles_and_flags` (`0x140227140`) into
`convert_tileset_tiles_to_megatiles_and_flags` (`0x14022e7b0`). At `0x14022e8b2`, the
converter tests the sign of the 16-bit source tile (bit 15); if set, it marks the megatile
and ORs runtime tile flags with `0x400000` at `0x14022e8bd`. `is_walkable_position`
(`0x1401d1120`) tests that flag at `0x1401d116e` and branches directly to a true return
at `0x1401d11c2`. Both decompiled dataflow and these machine instructions were inspected.

This differs from OpenBW's [tile_id predicate](https://github.com/OpenBW/openbw/blob/master/game_types.h),
which shifts a 16-bit value right by four and then masks bit 15, always returning false.
We follow the inspected game's behavior here. A high bit in malformed MTXM can therefore
open a whole tile, consistent with that conversion. This verifies the particular flag path
in this binary, not full engine pathfinding parity; later map-edge masking and other runtime
rules remain separate (see below).

`terrain_buildable` means only that the CV5 group lacks `UNBUILDABLE`. Occupancy, footprint,
resource exclusion, creep/power, map-edge rules, and race-specific placement checks are not
included. Neither elevation nor buildability changes route costs; only resolved walkability
determines connectivity. The last map row receives no special runtime exclusion.

Malformed shapes, absent table references, and unsupported normalized elevations are errors.
The WASM byte adapter additionally rejects empty tables and incomplete records. Upstream CHK
parsing remains permissive (including its terrain padding); this is not a map-validity checker.
Missing UNIT/THG2 chunks contribute no obstacles. These accessors currently report only
`ChunkMissing`; their parsers ignore incomplete trailing records rather than reporting corruption.
The WASM adapter matches that error explicitly, so future error variants require a decision.
An obstacle count of zero is not proof that the map object data is complete.
Map and terrain data must correspond: structurally valid tables from a different tileset or
mod cannot be detected from the table bytes themselves.

## Route contract

A deterministic eight-neighbor A* search uses costs 1000 orthogonal and 1414 diagonal.
A diagonal requires both adjacent orthogonal cells to be walkable. The octile heuristic
uses the same costs; ties are ordered explicitly. Reported logical-pixel distance is total
cost times 8/1000, a fixed-point approximation rather than exact Euclidean length.
Air distance is Euclidean distance between the same snapped endpoint centers.

A successful route includes both endpoints. A valid identical pair has one point and zero
length. Invalid or blocked endpoints are errors and are never silently snapped elsewhere.
Disconnected valid endpoints return `None` (null ground distance in WASM JSON).

These are point routes through terrain with optional static obstacles. Collision rectangles
conservatively block every intersecting 8-pixel cell, so tight gaps can be overblocked.
Dynamic units, mover clearance, mineral walking, and runtime path selection remain absent.
The browser defaults to map obstacles present and can ignore all obstacles for comparison;
that toggle includes invincible objects and is not a destruction simulation. No result certifies that a particular unit can cross a gap or that
a wall is tight. Terrain dimensions are bounded to 256x256 map tiles / 1024x1024 walk cells;
route state is allocated per request. The worker keeps one route request in flight in the UI.

## Clearance field

`TerrainGrid::clearance()` measures the largest empty, axis-aligned square centered on each
8px walk-cell center. Its radius is the distance from that center to a side of the square,
in logical pixels. The square's interior cannot overlap blocked cells or extend outside the
map; touching their boundaries is allowed. Blocked cells have radius zero. Walkable cells next
to a blocker or map edge have radius 4, then 12, 20, and so on. An entirely open map therefore
still has finite clearance, bounded by its edges.

This is a Chebyshev (square) distance field of the resolved raster, not Euclidean distance or
corridor width. It does not certify a unit's movement or a wall: mover shape, collision rules,
and the conservative 8px obstacle rasterization still matter. Terrain buildability, elevation,
and ramps do not independently restrict clearance. Apply `with_obstacles` before computing
clearance to include the desired static objects; the source grid is never changed.

`ClearanceField` owns dimensions and row-major `u16` radii. `radius_pixels(WalkPosition)`
returns `None` outside the grid, distinct from a blocked cell's `Some(0)`. The field supports
borrowed and consuming buffer access. Two deterministic raster passes compute minimum
8-neighbor distance to blocked or virtual outside cells, then convert distance `d` to
`8*d - 4` for walkable cells. Work is linear in the number of cells, with one 2 MiB result
buffer at the maximum 1024x1024 shape and no per-cell heap queue.

WASM `clearancePixels()` returns an independently owned `Uint16Array` for the active obstacle
mode. The demo transfers it alongside flags when analyzing terrain or changing obstacle mode,
so the two overlays describe the same snapshot. The generated bindings copy each result out
of WASM memory; the worker transfers that owned buffer directly without another array copy.
Select **Clearance** for the heatmap; the numerical radius appears in hover details for every
overlay. The color scale defaults to 4-256px (orange through teal to violet) and has explicit
64/128/256/512px maximum presets. The legend follows that setting; saturated colors do not cap
the stored values. Changing the color scale only repaints the field, without recomputing it.
Existing routes and base placement do not use this field yet.

## Regions and candidate passages

`TerrainGrid::regions(&RegionOptions)` partitions the active walkable raster using clearance
peaks. `min_prominence_pixels` defaults to 32 and accepts 8 through 4096.
`min_relative_prominence_percent` defaults to 40 and accepts 0 through 100; zero disables the
relative filter. A losing peak survives only if the clearance drop before connecting to a stronger
peak satisfies **both** the absolute threshold and that percentage of its own peak clearance.
For example, a 200px peak connecting at 160px clears the 32px threshold, but its 20% narrowing
fails the default relative threshold. A 60px peak connecting at 20px passes both. This suppresses
splits caused by shallow dips in broad rooms, including those around resources. The strongest
peak in each disconnected component always survives. Equal-height peaks prefer the earlier
row-major position; flat plateaus do not produce one seed per cell. Increasing either threshold
keeps fewer peaks and therefore produces no more regions, although individual boundaries
and IDs can change. This is an exploratory terrain partition, not a base-territory classifier.

The first pass activates cells in descending clearance order and joins active routing neighbors
with a disjoint-set structure, recording the prominence of losing peaks at their merge level.
Surviving peaks seed a priority flood across the same eight-neighbor graph used by routes,
including the prohibition on diagonal corner cutting. Flood priority favors a higher path
bottleneck, then shorter local flood distance, then lower region ID and cell index. Distance resets
to zero whenever a path reaches a lower clearance level, then accumulates octile steps while its
bottleneck stays at that level. This places competing fronts relative to the local narrowing
rather than the distance back to their room centers. It reduces the bias that pulled boundaries
away from entrances when neighboring rooms had unequal sizes. Ownership is final once a cell is settled. This is a
deterministic segmentation heuristic, not a globally optimal path metric or minimum cut. Every walkable cell gets exactly one
region; blocked cells get label 0. Each region is connected and contains its seed. Public IDs
start at 1 and follow seed row-major order. Results are deterministic for the same grid and
options, but IDs are local to that result rather than persistent map identities.

`RegionAnalysis` owns the row-major `u32` labels, region metadata (peak position, peak square
clearance radius, cell count), and a graph of candidate passages. Each adjacent region pair
has one representative crossing between two valid neighboring cells. Among crossings for
that pair, selection prefers the largest minimum endpoint clearance, with coordinate ties
resolved deterministically. The marker's `clearance_radius_pixels` is that square radius,
not a corridor width or a guarantee that a unit can pass. Several geographically distinct
entrances between the same pair currently share one representative. A region boundary can
also cross open ground; these markers are not automatically certified strategic chokepoints.

Regions use only walkability and clearance. Elevation, buildability, starts, resources, and
race-specific building rules do not independently force boundaries. To account for resources
and neutral objects, run on the grid produced by `with_obstacles`; the demo uses its current
obstacle mode. Terrain-only mode can help distinguish geological passages from object clutter.
The implementation is bounded by the existing 1024x1024 grid limit, uses O(N) memory, and
O(N log N) sorting/priority-queue work. It does not silently drop regions to fit a display cap.

WASM `analysis.analyzeRegions(minProminencePixels, minRelativeProminencePercent)` returns an owned
`RegionSnapshot`. The second argument is optional and defaults to 40; metadata records both settings.
`labels()` returns an independent `Uint32Array`; `metadataJson()` returns the small metadata
catalog without repeating the label raster. Free the snapshot when done. It remains valid
after the source analysis is freed or toggled, and computing regions does not modify routes
or base discovery. The worker transfers labels and frees its temporary snapshot in `finally`.

The demo's **Find regions** action displays colored regions and optional passage markers.
Peak labels are shown only for regions with at least 64 walk cells; smaller regions still
retain their colors, hover IDs, and graph entries. The peak prominence and minimum relative
narrowing controls let callers compare finer and coarser partitions. Changing it,
changing obstacle mode, or rebuilding/replacing terrain clears the old result; stale replies
cannot attach a previous partition to a new snapshot. Base descriptions associate a base's
existing route anchor with its region. A base whose route anchor is absent remains unassigned,
including in terrain-only mode; no replacement anchor or hypothetical obstacle removal is
inferred. Multiple bases can share one region. Main/natural labels and base territory remain
separate future steps.

The general peak-suppression/marker-flooding approach is informed by the
[scikit-image morphology documentation](https://scikit-image.org/docs/stable/api/skimage.morphology#skimage.morphology.h_maxima)
and [watershed documentation](https://scikit-image.org/docs/stable/api/skimage.segmentation#skimage.segmentation.watershed).
This implementation uses the project's routing graph and deterministic peak-persistence
rules; it does not import those libraries or claim identical segmentation results.

## Entrance-width experiment

`TerrainGrid::entrances(start, end, &EntranceOptions)` looks for a confined approach opening
into wider terrain along a directed survey route. It does not require a clearance peak on both
sides, and does not depend on region labels or their prominence settings. It returns evidence
for inspection; it does not yet cut regions, assign base territory, or certify a walling location.

The survey route favors clearance with edge cost `step + step * 64 / min(endpoint_radii)`, using
integer division and the usual 1000/1414 step costs. This discourages wall-hugging while preserving
the same legal walk graph. Reported route distance is its physical octile length, not the penalized
search cost. Its path can differ from ordinary shortest routes. Invalid/blocked endpoints are
errors; disconnected endpoints return a null route with no candidates.

`TerrainGrid::entrances_batch(queries, options)` returns the same directed results in query order,
sharing one search among all destinations with the same origin and one clearance field across
the batch. `EntranceSurvey::new(Arc<TerrainGrid>)` prepares a reusable immutable terrain survey;
its `batch` method reuses clearance across calls as well. Each search stops when all requested
destinations have been settled, or its reachable component is exhausted. Settled targets still
expand when another target remains. Heap ordering and equal-cost parent handling are unchanged,
so routes and candidate evidence match independent directed queries. Reverse queries remain
separate origins; a reversed forward route is not substituted for a reverse search.

Batches accept at most 256 queries and cap the combined returned route length at 2,097,152 walk
points, returning an error if either limit is exceeded. These are input/output limits, not a CPU
budget: each distinct origin can require a full-grid Dijkstra search, including when an unreachable
target requires exhausting its reachable component. In the worst case, 256 distinct origins mean
256 such searches. Callers needing responsive cancellation should submit smaller origin groups
and yield between batches; a synchronous call cannot be interrupted midway. Empty batches return
no results. Prepared surveys keep clearance but do not retain every origin's search tree or cache
results by options.

Candidate centers lie within the first `max_distance_pixels` logical pixels (default 1024,
valid 256..4096). Profiles need 128px of route behind and 192px ahead, including tangent checks;
that lookahead may extend beyond the center limit. At each eligible route cell it estimates
direction from nearby points, quantizes that direction to eight headings, and casts perpendicular rays to measure a cross-section. One normal
is held fixed across the candidate's entire comparison window. Each ray stops at a blocker, the
map edge, or 80 walk steps. Cardinal spans count 8 pixels per crossed cell; diagonal spans use
the same 1.414 approximation as routes. These are sampled raster spans, not exact physical widths
or engine collision measurements. Returned span endpoints are in logical pixels.

Samples behind the candidate must show a confined approach over 96px, ending at an opening
64..640px wide. The maximum approach span may be at most 130% of the candidate span. Samples
96, 128, and 160px ahead must all widen by at least 64px and `min_widening_percent` (default 25,
valid 0..200). The inside samples must hit terrain on both sides; map-edge and scan-limit hits
cannot supply those walls. Outward measurements may be lower bounds, explicitly marked in the
result. Direction checks reject sharp bends through the comparison window. Candidates within
256px of route distance compete, favoring smaller spans and stronger widening, with at most four
returned in outward order.

The WASM `entrancesJson(startX, startY, endX, endY, minWideningPercent?)` adapter always uses the
terrain-only grid. Its coordinates and route points are walk cells; candidate endpoints, spans,
and route distances are logical pixels. Resources and neutral buildings do not supply the
geological entrance walls. The core method can instead be called on any caller-supplied grid.
No base catalog, route, or obstacle mode is mutated by these queries. The WASM snapshot lazily
prepares one survey tied to its original terrain, shared by the single-query adapter and
`entrancesBatchJson(queriesJson, minWideningPercent?)`. Batch JSON is
`[[[startX,startY],[endX,endY]], ...]`, capped at 32 KiB, and the result is an array of the same
survey objects in input order. The terrain is shared through `Arc`, so preparation does not copy
the grid. Obstacle toggles do not invalidate this terrain-only cache; replacing the analysis
snapshot gives the new map its own cache.

In the demo, discover bases, select A and B, and choose **Inspect entrances at A and B**.
Both directed surveys run independently, so the selected order cannot hide an entrance near B.
Dashed routes show the surveyed paths; A1/B1 span labels identify the originating base and show
widths in build tiles, with approach and outward evidence in the result text. Changing region
thresholds does not change these results. Base selections, terrain, and entrance settings invalidate old results, and stale
worker replies cannot restore them. Bases without an existing route anchor cannot start a survey.

This is a local, direction-dependent detector. Another target can select another exit; parallel
bypasses are not ruled out. Winding entrances, transitions outside the sampling window, and
openings outside the span limits can be missed. Terrain-only evidence does not establish current
access through neutral blockers, building placement legality, or unit fit. The base-area experiment
below tests whether proposed spans can supply useful partition boundaries.

The initial real-map check covers Python 1.3 and 1.6, Hunters 2021, and Revolver SE 2.0.
On both Python versions, surveying the right natural toward the left natural finds a 320px span
with at least 400px ahead; the top natural toward the bottom main finds 392px with 528px ahead.
These are repeatable geometric observations, not ground-truth wall labels. Tests cover exact ray
endpoints, blocked diagonal corners, flat corridors, bends, disconnected endpoints, and capped
outward evidence. Fuzz checks enforce legal routes, physical distances, span bounds, widening,
candidate spacing, and determinism. The browser check exercises tuning, overlay visibility,
base changes, obstacle modes, and stale replies across map replacement.

## Base-area boundary experiment

**Test base areas at A and B** shades the terrain components containing the selected bases after
candidate entrance crossings are closed for partitioning. Ordinary routes and clearance regions
stay unchanged. This is a way to evaluate proposed base boundaries, not a finished map-wide
region classifier or a wall-placement solver.

`TerrainGrid::partition_by_spans(&[BoundarySpan])` accepts up to 256 finite segments in logical
pixel coordinates. It removes only existing legal walk-graph edges crossing those segments and
floods the remaining graph. Every original walkable cell retains a positive component label;
blocked terrain stays zero. No virtual obstacle is added to the source grid. Endpoints may lie
on the map edge, but must be in bounds and distinct. Component IDs follow row-major traversal.
Reversing endpoints, reordering spans, or duplicating spans leaves labels unchanged.

A graph vertex lies at its walk-cell center. Vertices exactly on a span's line belong to its
nonnegative signed side after lexicographically ordering the endpoints. This consistent tie rule
avoids manufacturing an isolated row of vertices along the cut. Diagonal edges obey the original
no-corner-cut movement rule. Crossings are tested against the finite segment, not its infinite
supporting line. Candidate edges are enumerated along the span's major axis, with at most seven
minor-axis cells visited per column or row. Integer interpolation and a conservative margin
include crossings at endpoints and grid corners; the exact finite-segment predicate still decides
which edges to remove. Sweep work therefore grows with total span length rather than bounding-box
area. A full component flood follows once after all spans; repeated spans retain separate boundary
assessments but do not change the final labels.

Each input span reports removed edge count, the count whose endpoints have different final
labels, and sorted distinct component pairs. These effects are measured **with all cuts applied**:
two exits can jointly enclose an area even though either alone has a bypass. A removed edge whose
ends remain connected has a surviving bypass. A span can have both separating and bypassed
crossings; it is not automatically validated as a chokepoint. Nor does a finite supplied span
necessarily terminate at real walls: callers proposing their own spans must evaluate that evidence.

The WASM `partitionAreas(spansJson)` method always uses terrain alone, independently of the
obstacle toggle. JSON is an array of `[[x1,y1],[x2,y2]]` spans, with a 32 KiB input limit. The
owned `AreaSnapshot` provides a fresh `Uint32Array` via `labels()` and compact `metadataJson()`;
call `free()` when done. Invalid inputs leave the source analysis unchanged. The empty-span
partition is lazily cached on the immutable terrain snapshot; returned snapshots share its
read-only storage while `labels()` still returns a fresh owned array.

The demo surveys the selected pair plus each selected base's three nearest anchored bases in
both directions (at most 14 directed surveys submitted as one batch). Nearest means
squared straight-line anchor distance with base-ID tie breaking, not a strategic neighbor claim.
Exact duplicate spans are consolidated while preserving their source surveys. All proposed spans
are then applied together. The selected areas are cyan and purple, or amber if they share a
component. Green span lines separate components; dashed amber lines retain bypassed crossings;
gray lines cross no legal graph edges. The summary reports area IDs, sizes, other known base
anchors in each component, and shrinkage relative to uncut terrain connectivity. Threshold, map,
base, and obstacle changes invalidate both area data and cached textures; stale replies cannot
restore them. Region prominence settings do not affect the experiment.

On Python 1.3 and 1.6 at 25% widening, the selected left and top naturals produce components of
5706 and 6040 walk cells, each containing only its own known base anchor. The right natural gives
5495 cells. Main-ramp spans provide the inner boundaries and the widening spans provide the outer
boundaries. A sampled span toward the center remains bypassable and is marked accordingly.
These are regression observations, not authoritative human base-area labels.

Coverage remains deliberately local: bases without route anchors are skipped, nearby disconnected
bases can consume survey slots, and other exits can be missed. Parallel but nonidentical spans
can leave small intermediate components; this checkpoint does not merge or classify those.
Containing only one known base anchor does not prove a strategically correct base area. The next
step is evaluating and consolidating boundary evidence across more approaches before feeding it
into the general region partition.

## Resource bases and depot candidates

Discovery proposes one depot site per substantial mineral field, balances mineral and gas access,
and associates each start with at most one field. A candidate can declare minerals or buildings
that must be cleared. Only those declared objects are removed when scoring its access; current
route queries retain the map's objects. These are suggested sites and point-grid distances, not
validated building placements or simulated mining rates.


`discover_bases` accepts a resolved terrain grid, normalized `ResourceNode` collision rectangles,
**non-resource** `StaticObstacle` records (bounds and destructibility), start positions in pixels,
and `BaseSearchOptions`. Resources include an optional remaining amount; the solver adds their occupancy itself. The core has no stock
unit IDs. The default depot footprint is 4x3 build tiles, shared by stock Command Center, Hatchery,
and Nexus. Results contain the depot's top-left build tile, an optional walk-cell route anchor,
resource/start indices, and indices of objects that must be cleared for placement. Indices
refer to the original input slices. WASM IDs follow sorted depot positions (row, then column) and
are snapshot-local.

Known mineral patches containing at most eight minerals are potential clearing prerequisites by
default. They do not contribute to clustering, resource counts, or mining scores. A candidate may clear only patches intersecting its resource-exclusion gap; all other patches
remain in that candidate's scoring grid. **Current routes still include every patch**. Set
`max_mineral_blocker_amount` to `None` to disallow mineral clearing, or choose a different threshold.
Unknown amounts and gas nodes are never eligible. `required_mineral_indices` lists the patches
intersecting the chosen depot's resource-exclusion rectangle, not every patch that could affect
mining routes. Any patch not listed stays blocked during scoring. Omitting a patch does not by
itself remove an overlapping non-resource object.
Overlapping destructible buildings can also be clearing prerequisites (disable with
`allow_destructible_clearing = false`). Only buildings intersecting the proposed footprint are
omitted; other buildings remain in that candidate's access-scoring grid. Candidates sharing both a cleared-building set and a cleared-mineral set reuse the same scoring
grid and resource searches. `required_obstacle_indices`
refers to the original non-resource obstacle slice, preserving distinct overlapping objects.

An exact start-mapped candidate additionally allows overlapping non-resource objects to be
removed by startup, even if invincible. These appear separately in `start_cleared_obstacle_indices`.
Disable this with `allow_start_obstacle_clearing = false`. This is conditional on that start being
occupied in a melee game: discovery does not know the active lobby assignments and does not remove
objects from current routes. Resource nodes continue to obey the resource model above; the full
engine startup process (which can also remove overlapping resources and moving units) is not
simulated. Exact start associations survive even when their current route anchor is blocked.

These are explicit hypothetical analysis policies; the game's placement check does not exempt small
mineral amounts.

The heuristic is bounded and deterministic:

- Cluster resources using terrain-only connectivity between their walkable perimeter cells, within
  32 eight-neighbor steps with no corner cutting. Transitive clusters need at least four distinct
  mineral rectangles; gas is optional. Equal-cost steps here are distinct from reported route costs.
- Search depot top-left positions within 12 build tiles of the cluster mean, adjusted for footprint
  size. Every footprint cell must be terrain-buildable and walkable after stamping permanent
  resources and accounting for that candidate's obstacle-clearing prerequisites.
- Apply the stock three-tile resource spacing. For a footprint at `(L,T)` with dimensions `(W,H)`,
  the excluded **half-open** pixel rectangle is
  `[(L-3)*32, (L+W+3)*32) x [(T-3)*32, (T+H+3)*32)`. Touching its right/bottom endpoint is legal;
  these query endpoints do not receive the +1 used when normalizing inclusive units.dat bounds.
- Flood from each resource's available perimeter on the static scoring grid. Every resource must
  reach a non-start candidate footprint perimeter within a 96,000-cost search budget. Scoring uses
  1000/1414 orthogonal/diagonal costs, avoiding the broad diagonal ties of equal-cost steps.
  Resource weights use remaining amounts capped at the category's median known positive amount;
  unknown amounts use that reference, and weights are at least one. Low-yield outliers therefore
  contribute less than full patches. Minimize weighted mean mineral distance plus weighted mean
  gas distance (or just mineral distance for mineral-only bases). Equal scores prefer a weighted
  resource-center balance, then row/column. Gas therefore
  has equal category weight instead of being outvoted by the number of mineral patches.
- A start-mapped footprint needs access to at least one of its cluster's resources. Each start tile
  is assigned to one cluster by weighted mean distance to its reachable resources after declared
  clearing. The mean across present resource categories is compared at millionths of a fixed-point walk
  cost, so field sizes
  and total mineral amounts do not bias ownership. Buried individual patches do not disqualify a
  start. Geometric ties are independent of input order. Within the owning cluster, exact starts
  take precedence, favoring more reachable resources and then the score. Start mapping floors
  `(start_pixels - footprint_pixels/2) / 32`. Nearby starts associate within six build tiles of the
  depot center and a 48-step connection on the current all-object grid; a nearby start label goes
  only to its nearest eligible base. Owned start sites are selected first, and subsequent clusters
  try their next candidate if a depot tile is already used. Output depot tiles are unique.
  Adjacent candidates can still have overlapping footprints: discovery proposes sites independently,
  rather than validating a layout in which every suggested depot is built simultaneously.
- Choose the current walkable cell inside the footprint nearest its center, breaking ties north,
  then west. A fully blocked conditional footprint has no route anchor and cannot be used for
  base-route queries. Ground and air distances use the same anchors. The hypothetical depot itself
  is not inserted as a blocker.

Search scratch state is reused and footprint availability uses a prefix table. Each distinct
clearing set currently clones and rasterizes a whole grid. At most 625 candidate tiles are searched
per cluster, but many distinct overlapping blockers can still make large maps expensive; there is
no separate group-count cutoff. Candidate perimeter buffers are released before global selection. Inputs are limited
to 512 resources and 64 starts. Grid dimensions must be build-tile aligned and the footprint must
fit. Malformed, out-of-map, and duplicate resource rectangles are counted as ignored; the first
node at an identical rectangle wins. Valid resources without terrain access cannot join a cluster
but still exclude placements. Out-of-map starts are ignored without renumbering input indices.
A qualifying cluster with no candidate increments `unplaced_clusters`; small mineral groups and
gas-only groups do not count as bases. Elongated fields and unusual layouts can fall outside the
heuristic. Scoring approximates access, not optimal gathering; it does not certify mover clearance,
creep/power, full placement legality, base territory, or main/natural roles.

The WASM adapter classifies minerals 176-178 and raw geysers 188 using caller-supplied units.dat
collision bounds and the grounded-resource/UNIT/THG2 policy used by static obstacles. UNIT amount
overrides are retained; THG2 resource amounts are unknown. Refineries are not raw resource nodes;
their special resource-container exclusion is not modeled beyond static occupancy. Mods with
different IDs should use the normalized Rust API. `findBasesJson` accepts optional `depotWidthTiles`,
`depotHeightTiles`, `maxMineralBlockerAmount` (default 8; null disables mineral clearing),
`allowDestructibleClearing`,
and `allowStartObstacleClearing` (both default true). JSON candidates include `requiredMinerals`
with bounds/amounts, `requiredObstacles` and `startClearedObstacles` as half-open bounds arrays,
and nullable `routeAnchor`. Invalid requests leave the last successful catalog intact. Terrain-only snapshots reject discovery; resource-free
map snapshots return an empty catalog.

```js
const analysis = map.analyzeMap(cv5, vf4, unitsDat)
const catalog = JSON.parse(analysis.findBasesJson('{}'))
const reachableAnchors = catalog.bases.filter(base => base.routeAnchor !== null)
if (reachableAnchors.length >= 2) {
  const route = JSON.parse(analysis.baseRouteJson(reachableAnchors[0].id, reachableAnchors[1].id))
  // null groundDistancePixels means disconnected; airDistancePixels still exists.
}
```

The demo exposes labeled footprints, resource counts, starts, and base-pair comparisons. A `*`
marks sites requiring mineral or object clearing; selecting one highlights its prerequisite patches in
orange. Ground routes are orange and the dashed line is air distance. The obstacle toggle preserves
the catalog and recomputes the route. Changing maps or rebuilding terrain clears it.

The half-open spacing boundary was checked in the labeled `12310g.exe` functions
`check_building_placement_tiles` (0x1402b29c0; rectangle construction at 0x1402b2ba6) and
`find_unit_borders_rect` (0x1401b9c60; strict endpoint comparisons at 0x1401b9d08/0x1401b9d5c).
`add_to_position_search` (0x1401a2400) uses inclusive unit collision borders. Stock node IDs and
placeboxes were checked against `unit_is_resource_node` and the unit definitions, with
[OpenBW's placement and start loader](https://github.com/OpenBW/openbw/blob/master/bwgame.h) as a
second reference. Native exclusion uses the broader resource-container flag (0x2000). These checks
verify input and spacing conventions, not the complete candidate algorithm against gameplay.

Startup cleanup was checked in `create_starting_main_building` (0x1403acd80): it snaps the depot's
placement dimensions to build tiles and calls `for_each_unit_in_area` at 0x1403ace75. Its callback
`kill_unit_in_starting_building_area` (0x1403acd60) unconditionally calls
`kill_unit_without_death_animation`, including for invincible units. Cleanup applies when a starting
depot is actually spawned; a map's start marker alone does not establish removal.

## Validation and next checkpoints

The [base discovery profiling checkpoint](analysis-performance.md) records native/browser
latency, memory measurements, the installed-map inventory, and a repeatable native harness.

Clearance tests compare the transform with direct geometric minima on small grids, including
diagonal blockers, corridors, map edges, obstacle monotonicity, and the maximum grid. WASM
checks verify owned buffers and obstacle toggles; the terrain fuzzer checks clearance bounds
and local continuity.

Region tests cover flat plateaus, equal-height peak ties, disconnected cells, diagonal
pinches, corridor prominence, shallow room splits, unequal-room entrance boundaries, and
representative crossing selection. Fuzz checks independently
verify label coverage, connected regions, passage coverage and validity, deterministic repeats,
and nonincreasing region counts under either coarser prominence threshold. Local WASM checks
exercise these invariants on Lemon, Python 1.3/1.6, Hunters 2021, Revolver SE 2.0, Primeval Isles,
and Crystallis with installation-supplied assets. Hunters and both Python versions additionally
have sampled main-room points checked for consistent membership after relative filtering.
These samples and screenshots are regression observations, not complete annotated base boundaries.

Synthetic tests cover table/index resolution, global row layout across megatiles, aggregate
walkability thresholds, creep, malformed inputs, diagonal pinches, deterministic ties,
disconnection, and route cost versus an independent Dijkstra reference. WASM tests exercise
map parsing with synthetic metadata and the JSON/flag contracts. Real installation assets
are optional local smoke inputs and must not be committed.

Base tests additionally cover local connectivity across long detours, duplicate nodes, spacing
boundaries (including translated Python and Hunters layouts), invalid footprints, start association,
gas/mineral balance, permanent barriers, conditional mineral clearing,
current route anchors, invalid inputs, and JSON/cache behavior. The terrain fuzz target also exercises bounded resource discovery.
Local real-asset checks cover Lemon's mineral-only naturals and startup cleanup, and Python 1.3
and Hunters 2021 verify all start-aligned depots and Python
island prerequisites. Lost Temple and Horizon Lunar Colony also exercise candidate/route invariants
and UI behavior; their candidate counts are observations, not ground-truth annotations.

Future checkpoints can add per-object identity/removal, movement profiles, and region
refinement. Base sites, terrain regions, base territory, and a main/natural role relative to a start
remain distinct concepts.

Wall checking should precede wall search: inspect human-provided placements, choose a mover,
and show a leak route under a named model. Exact passage and placement legality need their
own compatibility work against the engine; a bounding box on raw minitiles is insufficient.
The shared solver can remain reusable while neobrood supplies live state and validation.

Implementation references include the local neobrood importer (`gen_rules/src/main.rs`,
`Bounds16::to_tokens`), its construct flags and map initialization, and broodmap's existing
UNIT/THG2 parsing. An independent [OpenBW source check](https://github.com/OpenBW/openbw/blob/master/bwgame.h)
of `load_map_data` and `refresh_unit_position` informed owner filtering and grounded add-ons.
OpenBW rejects disabled THG2 unit sprites as unimplemented; retaining their candidate boxes
is our conservative choice, not behavior verified against OpenBW. These are references, not engine conformance
proofs. The raw bounds convention is independently corroborated by
[BWAPI UnitType dimensions](https://bwapi.github.io/class_b_w_a_p_i_1_1_unit_type.html):
width is left + right + 1, and height is up + down + 1. Runtime collision exceptions and
door behavior still need independent game validation. The conservative 8-pixel rasterization
is this library's analysis model, not a claim about StarCraft's pathfinder.

Prior art: [BWEM](https://bwem.sourceforge.net/faq.html) for clearance-based regions and
chokepoints, [BWEB](https://github.com/Cmccrave/BWEB) for building arrangements and wall search.
These inform future work; this checkpoint does not port either implementation.
