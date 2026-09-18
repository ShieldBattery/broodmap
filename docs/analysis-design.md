# Terrain analysis experiment

This checkpoint adds resolved terrain, static map obstacles, resource-base candidates, and point/base
route comparisons. It is a foundation for regions and wall checking, not an engine-compatible
movement solver or a complete building-placement checker.

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

Future checkpoints can add per-object identity/removal, movement profiles, clearance fields, and
regions. Base sites, terrain regions, base territory, and a main/natural role relative to a start
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
