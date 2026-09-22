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
