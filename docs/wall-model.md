# `pixel-rect-v1` wall model

`broodmap_analysis::walls::check_wall` is a bounded, hypothetical passage check. It evaluates a
caller-proposed building layout with the named `pixel-rect-v1` model; it does not emulate the Brood
War movement state machine, its pathfinder, or complete building-placement legality.

## Contract

Every coordinate is an integer pixel. A mover with inclusive extents `left`, `top`, `right`, and
`bottom`, centred at `(x, y)`, occupies the half-open rectangle:

```
[x - left, x + right + 1) x [y - top, y + bottom + 1)
```

Each terrain walk cell is an 8 by 8 pixel rectangle. The checker blocks a centre when the mover
rectangle intersects an unwalkable terrain cell, a caller-supplied static obstacle, or a proposed
building collision rectangle. It also blocks a centre when the complete mover body leaves the map.
The query window limits where a centre may travel; it does not clip collision. Terrain and obstacles
outside the window can therefore block a mover whose centre remains inside it.

The search examines integer-pixel centres in a nonempty, in-map window of at most 1024 by 1024
centres (and at most 1,048,576 centres total). It uses eight neighbours. A diagonal is legal only
when both orthogonal side centres are clear, so the graph cannot pass through a blocked corner touch.
That is a deliberate graph rule for `pixel-rect-v1`, not a claim about engine diagonal motion.

Buildings carry separate geometry:

* a 32-pixel-tile placement footprint, used for diagnostics; and
* an inclusive collision extent converted to a half-open collision rectangle, used for passage.

Placement diagnostics report out-of-map footprint or collision geometry, unwalkable or unbuildable
footprint terrain, and footprint overlaps with supplied obstacles or other proposals. Every proposed
collision rectangle still participates in the passage calculation when its placement has issues.
This makes the passage result explicitly **hypothetical** rather than silently discarding an invalid
proposal.

Before adding proposals, the checker records whether the endpoints are connected under terrain and
caller-supplied obstacles. `BlockedWithinWindow` means proposals disconnect a passing baseline within
the supplied window. `AlreadyDisconnected` means the baseline failed. `StartBlocked` and `EndBlocked`
identify unsuitable post-plan endpoints. An `Open` result includes a deterministic route with the fewest graph steps.
Neither a local block nor an open route proves anything outside the query window.

## Engine reference evidence

The following evidence motivates the rectangle conventions and records the boundary between this
model and engine behavior. It is reference evidence, not a conformance claim. OpenBW links are pinned
to [`d5fe2306ecb08efdea877a7f4117b178292137cb`][openbw-tree].

OpenBW represents a unit box as `origin - dimensions.from` through
`origin + dimensions.to + (1, 1)`, a half-open representation of raw inclusive extents
([`unit_type_bounding_box`][openbw-bounds]). Its [`is_walkable`][openbw-walkable] query is a
centre-point check and resolves partially walkable terrain at 8 by 8 mini-tile granularity. But
[`unit_type_can_fit_at`][openbw-fit] also checks unit bounds and calls [`can_fit_at`][openbw-can-fit],
which expands terrain contours by all mover extents. A centre-only walkability check can therefore
accept a centre whose body clips a wall. `pixel-rect-v1` approximates this fit condition by expanding
blocked terrain rectangles by the mover extents.

OpenBW derives terrain obstacle data from non-walkable mini-tiles [openbw-minitiles], derives
cardinal boundaries [openbw-boundaries], and traces corner-aware contours [openbw-contours]. Actual
terrain movement uses swept contour checks over a proposed delta
([`check_unit_movement_terrain_collision`][openbw-swept]). Those contours and swept checks are
intentionally outside `pixel-rect-v1`; grazing a corner, diagonal sliding, high-speed crossing, and
sub-pixel movement are not engine-exact here.

Placement is likewise separate from movement geometry. OpenBW derives the footprint tile rectangle
from `placement_size / 32` and runs map/reachability checks before tile validation
([`can_place_building`][openbw-placement]). It validates terrain and special tile state
[openbw-placement-terrain] and separately checks occupancy and units [openbw-placement-occupancy].
Ground buildings mark their placement-size tile rectangle occupied [openbw-occupancy]. Thus footprint
geometry must not replace a collision rectangle while measuring a gap.

Binary Ninja inspection of `12310g.exe.bndb` (binary `879807300654330409:PE`) independently found:

* `unit_collision_rect_at_position` at `0x1401c6a00`: raw rectangle coordinates are `x - left`,
  `x + right`, `y - top`, and `y + bottom`.
* `unit_collision_width` at `0x1401c8350`: width is `right + 1 + left`.
* `rect_can_fit_at_position` at `0x1401d15e0` and `unit_type_can_fit_at_position` at
  `0x1401d1700`: map bounds and centre walkability feed into dynamic position clearance.
* `unit_movement_is_blocked_by_terrain` at `0x1401cfde0` calls
  `unit_pathing_blocks_movement_delta`, confirming the engine's movement decision is delta-based.

Local, uncommitted `target/wall-binja-*.json` HLIL exports retain the address-level evidence. These checks support the
inclusive extent convention and the distinction between centre walkability, fit, and swept movement;
they do not turn this bounded static model into an engine simulator.

## Deliberate exclusions

`pixel-rect-v1` does not decide creep, power, add-ons, refineries, resource/depot spacing,
fog/visibility, builder reachability, moving-unit collision, repulsion, engine contour flags, or
order-specific recovery. It also does not certify a global seal. Callers should present results as a
hypothetical `pixel-rect-v1` route check with placement diagnostics and baseline status.

## Validation checkpoint

Core tests include exact inclusive gap thresholds, asymmetric dimensions, diagonal corner blocking,
out-of-window terrain, map edges, placement/collision separation, baseline disconnection, hostile
coordinates, and 96 generated small cases checked against a direct rectangle-intersection/BFS
reference. The terrain fuzz target exercises routes, repeatability, mover bodies, diagonal side
positions, and invalid query/profile/capacity inputs. A final 31-second ASan smoke run completed
866 inputs without a finding (an earlier run before the rejection checks completed 1,088).

The workspace's 562 tests and the demo/reference's 88 Node tests pass, as do strict workspace clippy,
format checks, web/Node WASM builds, and the production demo build. A local Chromium run on Python
1.3 checked automatic map setup, centered cursor previews and placement, zoom/pan without stray
placements, manual and entrance-selected windows, recentering, valid/conflicting proposals, blocked
endpoints, undo/clear, and stale replies after obstacle/map changes. UI checks also covered narrower
windows, route inspection, rapid map replacement, and setup retry after failed asset requests.
These are model and integration checks, not in-game wall tests.

Real-asset Node WASM queries on Python 1.3 and Hunters 2021 used supplied Zergling/Depot dimensions,
1024-square windows, and short open routes around a proposal. Warm medians were approximately 11.5
and 11.3 ms respectively; Python's first query was 18.8 ms. These two open-route observations do not
bound a maze or disconnected search's latency.

[openbw-tree]: https://github.com/OpenBW/openbw/tree/d5fe2306ecb08efdea877a7f4117b178292137cb
[openbw-bounds]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L710-L721
[openbw-walkable]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L767-L780
[openbw-fit]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L8490-L8499
[openbw-can-fit]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L8501-L8533
[openbw-minitiles]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L20050-L20065
[openbw-boundaries]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L20087-L20095
[openbw-contours]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L20655-L20788
[openbw-swept]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L8800-L8972
[openbw-placement]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L2502-L2514
[openbw-placement-terrain]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L2551-L2571
[openbw-placement-occupancy]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L2573-L2617
[openbw-occupancy]: https://github.com/OpenBW/openbw/blob/d5fe2306ecb08efdea877a7f4117b178292137cb/bwgame.h#L15833-L15843
## Automatic search checkpoint

`walls::search::WallSearch` reuses this model to find removal-minimal layouts within a bounded
placement area. It prepares each mover's terrain/object baseline once, follows route witnesses
(including diagonal side cells), and uses a lazy depth-first traversal with deterministic candidate
ordering. Repeated geometry is interchangeable; overlapping footprints and invalid static placements
are excluded. See [the API and limits](analysis-design.md#automatic-wall-layout-search).

Validation includes a three-building corridor compared against exhaustive candidate subsets, optional
smaller-mover passage, endpoint and placement rejection, duplicate-inventory and stepping parity, and
worker cancellation/ownership tests. All 574 workspace tests pass, along with strict clippy and
formatting. A 31-second ASan smoke run completed 890 executions without findings; its input-derived
search checks compare incremental chunk sizes and independently verify returned layouts with
`check_wall`.

A local Node WASM probe surveyed the 15 Python 1.3 and 29 Hunters 2021 topology boundaries using
up to two Depots and one Barracks, Zealot-block/Marine-pass requirements, 768px route windows,
384px placement areas, a 500-state budget, and up to five suggestions. It returned 2 layouts across
2 Python entrances and 111 layouts across 24 Hunters entrances. Every returned layout was rechecked
for valid static placement, a passing baseline, Zealot blockage, and Marine passage through
`checkWallJson`. These are observations for these settings, not wallability labels for either map.

Local Node WASM timings including independent result rechecks had medians of 28ms on Python and
241ms on Hunters; the slowest observed search was 925ms, and the slowest incremental step was 15ms.
Fourteen searches reached the state budget. Browser scheduling adds yields between states, so these
measurements are not end-to-end browser latency guarantees. A Chromium workflow verified a real
Python suggestion, preview/dismiss preserving a manual plan, keep/recheck for both movers, and
cancellation on mode, obstacle, and map changes. The current demo preset is Terran; the core and
WASM API accept caller-supplied geometry.


### Ramp search endpoint correction

The Python 1.3/1.6 top ramp exposed a query-setup failure: the old 96px-normal A point
`[2016, 96]` overlapped the Zealot-expanded collision of a valid Depot at build tile `[60, 2]`.
The 16-tile search area contained 103 individually valid placements, but endpoint preservation
rejected useful branches and exhausted after eight layouts. Increasing the budget could not help.

Automatic endpoints now sample clear ground beyond the placement area plus collision/mover
expansion, pairing baseline components for both requested movers. The same site uses A
`[2220, 12]` and B `[1676, 436]`, with a lateral move into the upper room because the map edge
prevents a straight extension. The high-ground footprint filter leaves 39 candidates. On both
Python versions, the final query found five Zealot-blocking suggestions in 117 explored states,
or five suggestions in 194 states when Marine passage was also required. Each returned layout
was independently checked with `checkWallJson` for static placement, connected baseline, and the
requested post-plan passage. These are observed results for this query, not solver completeness
or game-engine compatibility claims.

Regression coverage includes asymmetric building overhang plus mover clearance, mixed footprint
heights, lateral room at the map edge, disconnected optional-mover components, invalid geometry,
and placeholder endpoint handling. Browser checks exercise nearest-entrance picking, high/low/
unrestricted elevation requests, preview adoption, manual-plan preservation, cancellation, and
stale replies after mode/obstacle/map changes. A broader 44-query Python/Hunters integration sweep
independently validated 103 returned layouts while developing the endpoint selection; these counts
are observations, not expected map labels. A 31-second ASan run completed 573 input-derived
`terrain_analyze` executions without findings.


### Entrance-relative placement reach

The demo's centered placement box is now replaced for selected entrances by a six-tile distance
from the finite entrance segment to each building's footprint center. Full footprints can extend
past this reach, while remaining inside the routing window on valid terrain and the selected
height. The optional `placement_site` rule belongs to the Rust analyzer; custom demo spots retain
the earlier rectangular mode. Automatic A/B points stay outside both the reach and all eligible
collision expansions, so unused ground can host test points without choosing the clear ramp lip
in front of every possible wall.

With six-tile reach and a 500-state budget, the north Python 1.3/1.6 ramp has 43 valid candidate
placements and returns five walls after 94 states (Zealot blockage) or 136 states (also preserving
Marine passage). The first result reproduces the known Depots at `[60, 2]` / `[63, 2]` and Barracks
at `[64, 4]`. All returned layouts pass the separate checker. Browser tests cover unchanged default
settings, high/low/either-side queries, preview adoption, and cancellation. The checkpoint passes
582 workspace tests, 97 Node tests, strict Clippy, formatting, and WASM/demo builds. A 31-second
ASan run completed 665 input-derived terrain-analysis executions without findings.


### Ramp classification and high-ground search

Python 1.3/1.6's left main ramp exposed an orientation error: unbuildable high-elevation cells at
its low end skewed the ramp-flag elevation centroids, so only generic entrance observations
survived. These had no ramp metadata and could not select high ground automatically. Structural
ramp orientation now prefers centroids of nearby, legally reachable buildable landings at each
height, retaining the existing finite-wall gates and centroid fallback when landings are missing
or coincide. The demo therefore receives an upper end and applies its existing elevation filter.

With two Depots and one Barracks, blocking Zealots and preserving Marine passage, this left ramp
has 90 individual placements at six-tile reach and exhausts after 556 states with one layout.
Eight-tile reach has 163 placements and returns five layouts in 1,654 states. Both Python versions
match; every returned layout passes the separate checker and stays entirely on high ground.
The north-ramp result remains five layouts in 136 states. The demo now defaults to 2,000 states;
500 remains available for a quick search. Larger reach can increase time to the first wall, and
budget exhaustion is not evidence that the entrance has no wall.

A nine-map topology comparison found no newly merged base groups. Hunters 2021, Luna 2.1,
Laplace 1.0, and Primeval Isles retained the same base-area masks; Python, Revolver, Lemon, and
Circuit Breakers gained structural ramp boundaries. These are regression observations, not a
claim that every ramp or strategic region is detected correctly.


The right Python main ramp exposed a second rejection: its outer flagged apron has no pair of
terrain walls. Resource obstacles had made that apron appear bounded in an early diagnostic;
structural ramp detection correctly ignores them. The existing shoulder search now also handles
an absent outer section, taking the first finite central section within the same inward-distance
and end-separation bounds. On this ramp it finds real walls at the fifth projection station.
All four main ramps on Python 1.3/1.6 now carry structural ramp metadata.

The right-ramp default high-ground search exhausts after 388 states with one Zealot-blocking,
Marine-passable layout, independently checked on both Python versions. At eight-tile reach it
finds one layout before the 2,000-state cap. A nine-map comparison against the landing-direction
checkpoint found no newly merged base groups; only Python and Circuit Breakers changed base-area
masks. Browser verification confirms the right-ramp picker activates high-ground search.

### Terrain-connected placement reach

Entrance-relative wall searches now discard terrain disconnected from the entrance inside the
local finite-segment radius and routing window. The radius still measures Euclidean distance to
a building's center; the additional walk-cell flood excludes nearby ledges across cliffs without
changing the meaning of reach. The public reach mask also drives the demo highlight. Ramp-side
elevation constraints remain separate, so explicitly choosing either side can still cross ramps.

On Python 1.6 entrance E5 outside the left natural, the default six-tile reach previously
highlighted 26 irrelevant high-ground tiles alongside 149 useful low-ground tiles. The shared
mask removes all 26 high-ground tiles and retains all 149 low-ground tiles; individual legal
building placements drop from 278 to 246. The excluded ledge also stays excluded at eight-tile
reach. Python 1.3/1.6 north, left, and right ramp searches retain their previous placement counts
and independently checked Zealot-blocking, Marine-passable layouts. Browser checks cover the
corrected E5 highlight, stale reach replies after a settings change, and either-side ramp search.

### Review hardening

The search now has a deterministic 64-case exhaustive oracle test with varied corridor geometry,
unbuildable construction cells, asymmetric collisions, repeated inventory, mixed footprints, and
an optional passage mover. The oracle enumerates legal placements and removal-minimal layouts
through the public checker independently of search pruning and requires exact equality without
hitting a state or result limit. Individually endpoint-blocking candidates are omitted from search,
while automatic endpoint selection retains the full placement union before choosing new A/B.

The worker replaces one timer per state with MessageChannel task yields around short batches.
A controlled browser run of the same 388-state Python 1.6 right-ramp query took 1,384ms with the
new loop versus 3,251ms with the old timer loop injected into the same worker. Both produced the
same independently checked Zealot-blocking, Marine-passable wall. These are local observations,
not a latency guarantee; a batch and constructor preprocessing cannot be interrupted.

Page-handler tests now cover job interlocks, stale progress/success/error replies, terrain and
entrance invalidation, and disabled inventory status. Reach previews debounce and coalesce pending
changes. Browser verification observed zero map-space texture repaints while panning and one
preview request for four rapid reach changes, and checked link-drop prevention and native Space
activation on a focused button. Adopting a suggestion explicitly replaces the manual plan.
