# Terrain analysis experiment

This checkpoint adds resolved terrain, static map obstacles, and click-to-click routes. It is a foundation for
regions, base candidates, and wall checking, not an engine-compatible movement solver.

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

## Validation and next checkpoints

Synthetic tests cover table/index resolution, global row layout across megatiles, aggregate
walkability thresholds, creep, malformed inputs, diagonal pinches, deterministic ties,
disconnection, and route cost versus an independent Dijkstra reference. WASM tests exercise
map parsing with synthetic metadata and the JSON/flag contracts. Real installation assets
are optional local smoke inputs and must not be committed.

Future checkpoints can add per-object identity/removal and movement profiles.
Clearance fields and regions can support
resource clustering and candidate HQ sites. Base sites, terrain regions, base territory,
and a main/natural role relative to a start should remain distinct concepts.

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
