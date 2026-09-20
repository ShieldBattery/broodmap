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
direction from nearby points, quantizes that direction to eight headings, and casts perpendicular
rays to measure a cross-section. One normal is held fixed across the candidate's entire comparison
window. Each ray stops at a blocker, the map edge, or 80 walk steps. Cardinal spans count 8 pixels per crossed cell; diagonal spans use
the same 1.414 approximation as routes. These are sampled raster spans, not exact physical widths
or engine collision measurements. Returned span endpoints are in logical pixels.

Samples behind the candidate must show a confined approach over 96px, ending at an opening
64..640px wide. The maximum approach span may be at most 130% of the candidate span. Samples
96 and 128px ahead must both widen by at least 64px and `min_widening_percent` (default 25,
valid 0..200). The 160px sample must also pass whenever its tangent remains aligned. A sharp
turn at that last station instead excludes it from width and lower-bound evidence; the
`outward_sample_count` / JSON `outwardSampleCount` field then reports two rather than three
samples. Earlier samples through 128px must still pass the tangent test, and missing tangent
support near route endpoints still rejects the profile. The inside samples must hit terrain on
both sides; map-edge and scan-limit hits cannot supply those walls. Outward measurements may be lower bounds, explicitly marked in the
result. Direction checks reject sharp bends through the required comparison window. Candidates within
256px of route distance compete. Full three-sample profiles rank before two-sample fallbacks,
then smaller spans and stronger widening win, with later route position breaking ties. This
evidence preference also applies to the four-candidate cap, so four full profiles can exclude
a distant fallback. Selected candidates are returned in outward order.

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

**Analyze all base areas** surveys the map once and caches a terrain partition made from
candidate entrance boundaries. Selecting A and B highlights their components without changing
the partition or rerunning searches. Ordinary routes and clearance regions stay unchanged.
This remains an experimental boundary detector, not a main/natural classifier or wall solver.

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

The demo chooses each anchored base's three nearest other anchors in the same original terrain
component and surveys each chosen connection in both directions. Nearest means squared
straight-line anchor distance with base-ID tie breaking, not a strategic neighbor claim. Islands
therefore do not consume unreachable neighbor slots. The query set is independent of A/B selection.
Each origin is submitted separately, sharing one search among its destinations; result routes are
released after extracting candidate evidence. Progress counts completed origins, including isolated
anchors with no neighbors. The worker yields between origin batches so cancellation can be handled
outside its serialized job queue. A synchronous search or partition cannot be interrupted midway;
a cancellation detected afterward discards its result before publishing or caching it.

Exact span duplicates are consolidated first. Nearby observations can then share a group only
when their corresponding endpoints are within 32 pixels and their directions differ by at most
15 degrees. Every pair in a group must pass, preventing chains of increasingly distant observations
from collapsing into one boundary. A known base anchor strictly between the supporting lines,
within both spans' extents, prevents consolidation. The narrowest measured member represents the
group, with endpoint ordering breaking ties; no averaged geometry is invented. Source pairs are
retained and sorted. This geometric heuristic reduces duplicate cuts, but does not prove that two
observations describe the same strategic entrance. More than 256 provisional boundaries fails
explicitly rather than silently dropping evidence.

Before partitioning, conflicting cuts whose interiors cross are resolved greedily: narrower
observed spans win, with canonical endpoint order breaking equal-width ties. Touching endpoints
and collinear overlaps do not count as crossing conflicts. Rejected spans are counted separately
and their sources are not attributed to winners; the pair inspector still exposes the raw
directional evidence. This prevents crossing room-spanning hypotheses from manufacturing tiny
regions, but is a selection heuristic rather than proof that the retained cut is strategically
correct. The 256-boundary cap applies before this filtering.

A provisional partition then exposes consecutive cuts through the same passage. A second,
conservative pass considers only chains of three or four spans whose intervening components have
no base anchor and exactly two incident boundaries. Every span must fully separate a single pair
of components and be a bridge in the full boundary multigraph, excluding alternate loops. Chains
must have distinct outer components and at most 384 pixels of total midpoint-to-midpoint length.
A single raster scan measures each intermediate component along and across its two span midpoints.
Its transverse extent must be at most 1.5 times the smaller span width plus 16 pixels; longitudinal
extent must be at most the midpoint distance plus half that width plus 16 pixels. Cell extents are
included. These guards reject broad rooms and long pockets rather than relying on empty base lists
alone. Failing a geometry guard rejects the whole candidate chain, not a smaller fragment of it.

Within an accepted chain, spans within eight pixels of its narrowest width are treated as near ties.
The representative minimizes summed midpoint distance to all chain members, with endpoint order
breaking ties. This favors a central neck over a similarly narrow oblique cut farther inside the
base. It uses an observed span, never an averaged line. All original observations and source pairs
remain attached as diagnostics. The partition is recomputed with only retained spans: discarded
cuts no longer trim base regions. Provisional labels and assessments are never published.

With terrain flags available, a further pass handles close parallel duplicates, including pairs.
Their intervening component must have no base anchor, exactly two incident boundaries, no ramp
cells, and one elevation throughout. Both cuts must fully separate a single component pair; unlike
the longer serial-chain rule, they may participate in a larger bypass loop. Entire cycles are
rejected. Groups contain two to four spans, every pair within 128 pixels at corresponding endpoints
and within 15 degrees of parallel. This complete-link rule prevents transitive long-chain merging.

The component must fit a slab aligned with the cuts, including walk-cell extents. Across the cuts,
the endpoint envelope allows 32 pixels of tolerance; along them, wall nooks may extend by at most
the larger of 32 pixels or one quarter of the narrower width. Measuring relative to the cuts avoids
mistaking sideways-shifted endpoints for a long corridor. The same near-minimum-width central
representative rule preserves original observations. Members of an accepted longer serial chain
are excluded from this pass, so provisional metadata remains sufficient for both decisions and
only one final repartition is needed. Ramp/elevation flag bits are immutable terrain attributes;
obstacle-dependent walkability flags do not affect this consolidation.

Ramp detection is independent of the confined-approach/widening predicate. `TerrainGrid::ramps()`
finds legal 8-connected components of walkable ramp-flagged cells and samples those cells plus their
immediate legal walkable border. It requires exactly two adjacent elevations, a direct elevation
transition, and at least 12 flagged cells. The ascent heading is the low-to-high centroid vector
quantized to eight directions using integer comparisons. Component cells are projected along and
across that heading. The longitudinal extent must be at least six projection units; a diagonal step
changes projection by two, while a cardinal step changes it by one.

Each end first examines a two-projection-unit band and selects the wall-bounded section nearest
its lateral centroid, then nearest its longitudinal extreme, then narrowest, with coordinate ties.
That initial section can already lie on the flat terrain beyond a ramp. A bounded inward search
examines central finite sections up to eight projection units from the extreme and accepts the
first that is at least 64 pixels and 25% narrower. It chooses centrality before testing width,
avoiding off-center nooks, and reserves the required gap between both ends. A consistently wide
ramp retains its original ends.

Ramp flags can also cover only the middle of the slope. For an unbuildable end, a bounded refinement
walks outward along the ascent axis for at most 16 legal steps, requiring the same elevation and
a buildable landing at that elevation. It samples full wall sections on the intervening unbuildable
cells. The first opening must widen by at least 64 pixels and 75%, and remain within both 384 pixels
and four times the original width. A larger or unbounded opening retains the last narrow section;
it does not search beyond that opening for another cut. Without a verified landing, the original
flagged end remains. These bounds prevent the flat unbuildable terrain around a ramp from pulling
its boundary across an unrelated room. Refined centers can lie outside the ramp-flagged cells.

Both sections need real terrain wall contacts, distinct geometry, and separation of at least four
projection units. Edges and scan caps cannot supply walls. The existing entrance ray sampler
supplies full unclipped endpoints, including the same diagonal corner rules. This refinement is
still heuristic: it does not promise the exact visual or buildability boundary of a slope. Ambiguous,
short, or unusual flagged shapes may be missed; these are structural terrain observations, not
certified connectivity or walling claims.

The WASM `rampsJson()` adapter lazily caches terrain-only evidence independently of obstacle mode.
Each recognized ramp has an ID, elevation pair, flagged-cell count, and `lower`/`upper` spans with
walk-cell positions and pixel endpoints/widths. The map-wide demo adds both ends after consolidating
directional entrance observations, labels them `R{id}L`/`R{id}U`, and protects them from serial/flat
duplicate removal. Ramp spans take priority over generic widening spans in crossing conflicts.
The 256-span limit includes both ends. A difficult crossing between two structural ramps remains
a geometric conflict rather than a guarantee that every recognized ramp survives intact.

A nearby ordinary mouth can be replaced by a ramp end when both cuts fully separate components
and share an anchor-free exterior approach. The region between that ramp's own two ends cannot
qualify. The mouth must be at least twice as wide, or have corresponding endpoints within 32 pixels
and direction within 15 degrees of the structural end. Proximity uses finite segment distance,
at most 224 pixels, rather than midpoint distance. Every corner of the absorbed approach's bounding
box must lie within 512 pixels of the ramp midpoint. A broad approach with only the mouth and
matching ramp as exits can instead bound each corner within 512 pixels of either cut's midpoint.
This includes a strict multipart star whose additional inner components are mouth-only leaves,
as described below. It accommodates a turn and pockets behind minerals without extending the
exception to branched approaches or near-coincident replacements.

A multipart mouth is eligible only when its component pairs form a star with one common outer
component. One inner component must meet only the mouth and matching ramp; every other inner
component must be a leaf incident only to that mouth. All inner components must be anchor-free
and satisfy the same bounds. This permits tiny terrain pockets without absorbing another exit.
Near-coincident spans can also produce a three-component triangle: both cuts have two component
pairs, one pair is shared, and their union has exactly three distinct pairs. Within the same
32-pixel/15-degree envelope, the generic cut may be removed if that entire possible union contains
at most one anchored component and every unanchored component is locally bounded. The selector
reserves the whole union against other removals; it does not infer which shared edges would be
restored. A fresh partition establishes the actual result.

Proposals rank by segment distance and canonical endpoints. They cannot reuse a mouth, logical
ramp end, or absorbed component, or absorb another selected proposal's outer component. Removed
mouths do not contribute their widening evidence to the ramp. Exact coincident geometry is also
retained only once, preferring structural evidence.

A further ramp-end duplicate case permits up to 64 pixels of perpendicular offset and 15 degrees
of angular difference, with projected overlap covering at least 75% of the shorter section and
a maximum width ratio of 1.5. Both ordinary-cut components must be anchor-free, and the shared
approach must have exactly two ports: the ordinary cut and this ramp end. The opposite ramp end
cannot also touch the approach. The same local bounds and reservation rules apply; parallel
geometry alone never permits removal.

Replacement runs at most twice, repartitioning after each changed pass. The fresh anchor labels
allow a near-coincident cut to be removed first and then expose a second redundant approach inside
the base, without allowing a chain of simultaneous removals to join known base components.

After ramp-mouth replacement, the demo can collapse generic cuts sealed between the lower and
upper ends of one ramp. It builds a graph of fully separating ordinary cuts and accepts only
anchor-free components with at most eight ordinary boundaries and exactly two structural ports,
both belonging to that ramp. Every pair on every removed boundary must lie inside the component;
every pair on each retained ramp port must cross from inside to outside. A multipart ramp port
may additionally touch an anchor-free leaf whose only incident boundary is that same port. Such
leaves enter the validation set and must satisfy the same elevation and local bounds; their
structural cut remains. This matters for multipart spans that also intersect unrelated terrain: the entire physical cut is retained unless all of it
qualifies. The merged interior must contain ramp cells, contain only the ramp's two elevations,
and fit within 512 pixels of either ramp-end midpoint. All component statistics share one raster
scan, and a single fresh partition applies the accepted removals. Both structural ends remain.

Finally, the demo can remove an internal junction cut whose two adjacent components contain no
ramp cells. Without base anchors, their union must have one elevation. At most one of the two
components may contain base anchors; in that case, both components must share an elevation
covering at least 99% of each component's cells. This tolerates a few differently flagged edge
cells, not a substantial slope. The removed cut must also be at least as wide as every retained
exterior port, preventing this rule from removing a narrower entrance into a base. The ordinary cut must
fully separate one component pair. At least three other retained physical spans must lead to at least
three distinct exterior components; each must be a full single-pair boundary within 384 pixels of
the candidate midpoint. Every corner of each adjacent region's bounding box must lie within
512 pixels of the candidate midpoint for unanchored unions, or of the finite candidate segment
for a union containing an anchored component. Port midpoints retain the 384-pixel midpoint bound
in both cases. Region bounds and terrain attributes are collected once, avoiding per-proposal grid
scans. Candidates rank widest first with canonical endpoint ties. Selected pairs are disjoint and
reserve all their exterior ports so simultaneous removals cannot erase the exits used as evidence.
A final partition reflects the merged junctions. This is a bounded heuristic for open junctions,
not proof that every remaining segment is a strategic chokepoint.

A second junction case handles a small flat area outside a base with two connections to the same
exterior region. The area must be unanchored, single-elevation, free of ramp cells, and bounded
within 512 pixels of the candidate midpoint. It must have exactly three fully separating ordinary
cuts: two toward one exterior region, and one toward a distinct anchored base component. A wide
cut may be removed only when it is at least twice the width of the other connection to the same
exterior, and both retained port midpoints are within 384 pixels. The narrow connection and the
base-facing entry stay. Candidate areas and their retained ports are reserved, and no selected
area may be another proposal's exterior. The exterior itself may be large or contain anchors;
only the small unanchored area is absorbed. Repartitioning can mark the retained narrow bridge
as bypassable, since the wide connection is now open; the displayed assessment reflects that.

All retained spans are applied together. The selected areas are cyan and purple, or amber if they
share a component. Green span lines separate components; dashed amber lines retain bypassed
crossings; gray lines cross no legal graph edges. Sections with no terrain-buildable cell within
64 pixels (two build tiles) of their midpoint are also muted gray, retaining their bypass dash
pattern. This is a presentation hint and never changes the partition. The test measures distance
to cell rectangles, ignores occupancy and elevation, and does not check building footprints,
creep, resource exclusion, or wall feasibility. Buildability near the midpoint does not establish
that a section can be walled. Ramp ends and unbuildable passages still carry terrain-topology
information. The summary reports area IDs, sizes, other known base anchors in each component, and shrinkage relative to uncut terrain connectivity. A successful
result contains metadata for every anchored base, not just the highlighted pair.

The worker retains one completed result keyed by widening threshold and the base-anchor catalog
on its terrain snapshot. Transferable labels are copied for delivery so the cached buffer remains
owned. Failed or cancelled runs never replace it. The page keeps its own result for selection-only
redraws. Changing threshold or rediscovering bases clears displayed areas; replacing terrain clears
both caches. Terrain-only areas survive obstacle toggles and region prominence changes. Separate
A/B entrance surveys do not change the map-wide result. Worker identity, terrain/catalog snapshots,
and request revisions gate progress and final replies after invalidation or map replacement.

On Python 1.3 and 1.6 at 25% widening, the selected left and top naturals produce components of
5706 and 6040 walk cells, each containing only its own known base anchor. The right natural gives
5495 cells. Main-ramp spans provide the inner boundaries and the widening spans provide the outer
boundaries. A sampled span toward the center remains bypassable and is marked accordingly.
The bottom natural's three outer cuts consolidate to the middle horizontal span at pixel y=3388,
restoring 1295 walk cells and increasing its area from 4768 to 6063 cells on both versions. Its
main-ramp boundaries remain separate. These are regression observations, not authoritative human
base-area labels.

Coverage remains deliberately local even though it is sampled across the map: bases without route
anchors are skipped and three neighboring bases do not expose every possible exit. Parallel but
nonidentical spans can still leave small intermediate components. Containing only one known base
anchor does not prove a strategically correct base area, and shared components are reported without
forcing an arbitrary split. Main/natural classification and wall-placement legality remain separate
future work.

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
