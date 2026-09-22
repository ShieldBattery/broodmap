//! A bounded, static wall check using the `pixel-rect-v1` collision model.
//!
//! This module intentionally does not claim to reproduce StarCraft's movement engine.  It tests
//! integer pixel centers against rectangular terrain and object blockers, with a caller-provided
//! rectangular mover.  Dynamic unit state, creep, power, resource/depot spacing, and engine
//! contour movement are outside this first model.

use std::collections::VecDeque;

use thiserror::Error;

use crate::{DepotFootprint, PixelPosition, PixelRect, TerrainGrid, TilePosition};

const MAX_WINDOW_AXIS: usize = 1024;
const MAX_WINDOW_CENTERS: usize = 1_048_576;
const MAX_BUILDINGS: usize = 32;
const MAX_OBSTACLES: usize = 8_192;
const MAX_COLLISION_EXTENT: u32 = 8_192;
const MAX_FOOTPRINT_TILES: u32 = 256;
const PIXELS_PER_TILE: i64 = 32;
const PIXELS_PER_WALK_CELL: i64 = 8;

/// Inclusive pixel extents around an integer pixel center.
///
/// In the `pixel-rect-v1` model a center `(x, y)` occupies
/// `[x - left, x + right + 1) x [y - top, y + bottom + 1)`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CollisionBounds {
    pub left: u32,
    pub top: u32,
    pub right: u32,
    pub bottom: u32,
}

/// One proposed building in a static wall plan.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct WallBuilding {
    pub tile: TilePosition,
    pub footprint: DepotFootprint,
    pub collision: CollisionBounds,
}

/// A bounded `pixel-rect-v1` wall query.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallQuery {
    /// Half-open pixel rectangle containing every center considered by the route search.
    pub window: PixelRect,
    pub start: PixelPosition,
    pub end: PixelPosition,
    pub mover: CollisionBounds,
    pub buildings: Vec<WallBuilding>,
}

/// A placement diagnosis for one proposed building.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PlacementIssue {
    /// The footprint or collision rectangle leaves the map.
    OutOfMap,
    /// At least one footprint walk cell is unwalkable or terrain-unbuildable.
    UnbuildableTerrain,
    /// The footprint intersects the indexed caller-supplied static obstacle.
    OverlapsObstacle { index: usize },
    /// The footprint intersects the indexed other proposed building's footprint.
    OverlapsBuilding { index: usize },
}

/// Static placement evidence for one requested building.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallPlacement {
    pub footprint: PixelRect,
    pub collision: PixelRect,
    pub issues: Vec<PlacementIssue>,
}

/// The result of the post-plan bounded passage search.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum WallPassage {
    /// The proposed collision boxes leave a route inside the query window.
    Open,
    /// The proposed boxes disconnect an initially connected pair inside the query window.
    BlockedWithinWindow,
    /// The endpoints were already disconnected before proposed buildings were added.
    AlreadyDisconnected,
    /// The post-plan start center is blocked.
    StartBlocked,
    /// The post-plan end center is blocked.
    EndBlocked,
}

/// Result of a static `pixel-rect-v1` wall check.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallCheck {
    pub placements: Vec<WallPlacement>,
    pub passage: WallPassage,
    /// Whether the endpoints connected before adding proposed collision boxes.
    pub baseline_passable: bool,
    /// A post-plan route, including both endpoints, when [`WallPassage::Open`].
    pub route: Vec<PixelPosition>,
    pub window: PixelRect,
}

/// Which requested endpoint is outside the bounded center-search window.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum WallEndpoint {
    Start,
    End,
}

/// Invalid input to [`check_wall`].
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum WallError {
    #[error("wall query window must be nonempty and inside the map")]
    InvalidWindow,
    #[error("wall query window is {width}x{height}; each axis is capped at 1024 pixels")]
    WindowTooLarge { width: usize, height: usize },
    #[error("wall query window has {centers} centers; the cap is {MAX_WINDOW_CENTERS}")]
    TooManyCenters { centers: usize },
    #[error("{endpoint:?} endpoint ({x}, {y}) is outside the wall query window")]
    EndpointOutsideWindow {
        endpoint: WallEndpoint,
        x: u32,
        y: u32,
    },
    #[error("wall query has {actual} proposed buildings; the cap is {MAX_BUILDINGS}")]
    TooManyBuildings { actual: usize },
    #[error("wall query has {actual} static obstacles; the cap is {MAX_OBSTACLES}")]
    TooManyObstacles { actual: usize },
    #[error("static obstacle {index} is empty or inverted")]
    InvalidObstacle { index: usize },
    #[error(
        "proposed building {index} has an invalid footprint of {width_tiles}x{height_tiles} tiles"
    )]
    InvalidFootprint {
        index: usize,
        width_tiles: u32,
        height_tiles: u32,
    },
    #[error("proposed building {index} has a collision extent above {MAX_COLLISION_EXTENT} pixels")]
    CollisionExtentTooLarge { index: usize },
    #[error("mover has a collision extent above {MAX_COLLISION_EXTENT} pixels")]
    MoverExtentTooLarge,
    #[error("proposed building {index} cannot be represented in pixel-rect-v1 coordinates")]
    CoordinateOverflow { index: usize },
}

/// Checks static building placement and a bounded, hypothetical `pixel-rect-v1` passage.
///
/// Placement diagnostics are intentionally separate from the passage calculation: every proposed
/// collision rectangle participates in the hypothetical passage, even if that building is out of
/// map, unbuildable, or overlaps another object.  This is a static rectangle model, not an
/// engine-valid placement or pathfinding claim.
pub fn check_wall(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    query: &WallQuery,
) -> Result<WallCheck, WallError> {
    let map_width = i64::from(grid.width()) * PIXELS_PER_WALK_CELL;
    let map_height = i64::from(grid.height()) * PIXELS_PER_WALK_CELL;
    let window = validate_query(map_width, map_height, obstacles, query)?;
    let geometries = validate_buildings(&query.buildings)?;

    let mut placements: Vec<WallPlacement> = geometries
        .iter()
        .map(|geometry| WallPlacement {
            footprint: geometry.footprint,
            collision: geometry.collision,
            issues: Vec::new(),
        })
        .collect();
    assess_placements(
        grid,
        map_width,
        map_height,
        obstacles,
        &geometries,
        &mut placements,
    );

    let width = usize::try_from(window.right - window.left).expect("validated window width");
    let height = usize::try_from(window.bottom - window.top).expect("validated window height");
    let mut base_difference = DifferenceGrid::new(width, height);
    stamp_terrain_blockers(grid, window, query.mover, &mut base_difference);
    for obstacle in obstacles {
        stamp_expanded(*obstacle, window, query.mover, &mut base_difference);
    }
    let base_counts = base_difference.finish();

    let mut plan_difference = DifferenceGrid::new(width, height);
    for geometry in &geometries {
        stamp_expanded(
            geometry.collision,
            window,
            query.mover,
            &mut plan_difference,
        );
    }
    let plan_counts = plan_difference.finish();
    let baseline = blocked_centers(
        &base_counts,
        width,
        height,
        window,
        map_width,
        map_height,
        query.mover,
        None,
    );
    let post_plan = blocked_centers(
        &base_counts,
        width,
        height,
        window,
        map_width,
        map_height,
        query.mover,
        Some(&plan_counts),
    );

    let start = local_index(query.start, window, width);
    let end = local_index(query.end, window, width);
    let baseline_route = route(&baseline, width, height, start, end);
    let baseline_passable = baseline_route.is_some();
    let (passage, route) = if post_plan[start] {
        (WallPassage::StartBlocked, Vec::new())
    } else if post_plan[end] {
        (WallPassage::EndBlocked, Vec::new())
    } else if let Some(indices) = route(&post_plan, width, height, start, end) {
        (
            WallPassage::Open,
            indices
                .into_iter()
                .map(|index| PixelPosition {
                    x: u32::try_from(i64::from(window.left) + (index % width) as i64)
                        .expect("validated window coordinate"),
                    y: u32::try_from(i64::from(window.top) + (index / width) as i64)
                        .expect("validated window coordinate"),
                })
                .collect(),
        )
    } else if !baseline_passable {
        (WallPassage::AlreadyDisconnected, Vec::new())
    } else {
        (WallPassage::BlockedWithinWindow, Vec::new())
    };

    Ok(WallCheck {
        placements,
        passage,
        baseline_passable,
        route,
        window: query.window,
    })
}

#[derive(Debug, Copy, Clone)]
struct BuildingGeometry {
    footprint: PixelRect,
    collision: PixelRect,
}

fn validate_query(
    map_width: i64,
    map_height: i64,
    obstacles: &[PixelRect],
    query: &WallQuery,
) -> Result<PixelRect, WallError> {
    if obstacles.len() > MAX_OBSTACLES {
        return Err(WallError::TooManyObstacles {
            actual: obstacles.len(),
        });
    }
    for (index, obstacle) in obstacles.iter().enumerate() {
        if obstacle.left >= obstacle.right || obstacle.top >= obstacle.bottom {
            return Err(WallError::InvalidObstacle { index });
        }
    }
    if query.mover.left > MAX_COLLISION_EXTENT
        || query.mover.top > MAX_COLLISION_EXTENT
        || query.mover.right > MAX_COLLISION_EXTENT
        || query.mover.bottom > MAX_COLLISION_EXTENT
    {
        return Err(WallError::MoverExtentTooLarge);
    }

    if query.buildings.len() > MAX_BUILDINGS {
        return Err(WallError::TooManyBuildings {
            actual: query.buildings.len(),
        });
    }

    let window = query.window;
    if window.left < 0
        || window.top < 0
        || window.left >= window.right
        || window.top >= window.bottom
        || i64::from(window.right) > map_width
        || i64::from(window.bottom) > map_height
    {
        return Err(WallError::InvalidWindow);
    }
    let width = usize::try_from(window.right - window.left).expect("positive i32 difference");
    let height = usize::try_from(window.bottom - window.top).expect("positive i32 difference");
    if width > MAX_WINDOW_AXIS || height > MAX_WINDOW_AXIS {
        return Err(WallError::WindowTooLarge { width, height });
    }
    let centers = width.checked_mul(height).ok_or(WallError::TooManyCenters {
        centers: usize::MAX,
    })?;
    if centers > MAX_WINDOW_CENTERS {
        return Err(WallError::TooManyCenters { centers });
    }

    for (endpoint, position) in [
        (WallEndpoint::Start, query.start),
        (WallEndpoint::End, query.end),
    ] {
        let x = i64::from(position.x);
        let y = i64::from(position.y);
        if x < i64::from(window.left)
            || x >= i64::from(window.right)
            || y < i64::from(window.top)
            || y >= i64::from(window.bottom)
        {
            return Err(WallError::EndpointOutsideWindow {
                endpoint,
                x: position.x,
                y: position.y,
            });
        }
    }
    Ok(window)
}

fn validate_buildings(buildings: &[WallBuilding]) -> Result<Vec<BuildingGeometry>, WallError> {
    let mut result = Vec::with_capacity(buildings.len());
    for (index, building) in buildings.iter().enumerate() {
        let footprint = building.footprint;
        if footprint.width_tiles == 0
            || footprint.height_tiles == 0
            || footprint.width_tiles > MAX_FOOTPRINT_TILES
            || footprint.height_tiles > MAX_FOOTPRINT_TILES
        {
            return Err(WallError::InvalidFootprint {
                index,
                width_tiles: footprint.width_tiles,
                height_tiles: footprint.height_tiles,
            });
        }
        let collision = building.collision;
        if collision.left > MAX_COLLISION_EXTENT
            || collision.top > MAX_COLLISION_EXTENT
            || collision.right > MAX_COLLISION_EXTENT
            || collision.bottom > MAX_COLLISION_EXTENT
        {
            return Err(WallError::CollisionExtentTooLarge { index });
        }

        let left = i64::from(building.tile.x) * PIXELS_PER_TILE;
        let top = i64::from(building.tile.y) * PIXELS_PER_TILE;
        let right = left + i64::from(footprint.width_tiles) * PIXELS_PER_TILE;
        let bottom = top + i64::from(footprint.height_tiles) * PIXELS_PER_TILE;
        let center_x = left + i64::from(footprint.width_tiles) * (PIXELS_PER_TILE / 2);
        let center_y = top + i64::from(footprint.height_tiles) * (PIXELS_PER_TILE / 2);
        let collision_left = center_x - i64::from(collision.left);
        let collision_top = center_y - i64::from(collision.top);
        let collision_right = center_x + i64::from(collision.right) + 1;
        let collision_bottom = center_y + i64::from(collision.bottom) + 1;
        let values = [
            left,
            top,
            right,
            bottom,
            collision_left,
            collision_top,
            collision_right,
            collision_bottom,
        ];
        if values
            .iter()
            .any(|&value| value < i64::from(i32::MIN) || value > i64::from(i32::MAX))
        {
            return Err(WallError::CoordinateOverflow { index });
        }
        result.push(BuildingGeometry {
            footprint: PixelRect {
                left: left as i32,
                top: top as i32,
                right: right as i32,
                bottom: bottom as i32,
            },
            collision: PixelRect {
                left: collision_left as i32,
                top: collision_top as i32,
                right: collision_right as i32,
                bottom: collision_bottom as i32,
            },
        });
    }
    Ok(result)
}

fn assess_placements(
    grid: &TerrainGrid,
    map_width: i64,
    map_height: i64,
    obstacles: &[PixelRect],
    geometries: &[BuildingGeometry],
    placements: &mut [WallPlacement],
) {
    for (index, (geometry, placement)) in geometries.iter().zip(placements.iter_mut()).enumerate() {
        let footprint_in_map = rect_in_map(geometry.footprint, map_width, map_height);
        let collision_in_map = rect_in_map(geometry.collision, map_width, map_height);
        if !footprint_in_map || !collision_in_map {
            placement.issues.push(PlacementIssue::OutOfMap);
        }
        if footprint_in_map && !footprint_buildable(grid, geometry.footprint) {
            placement.issues.push(PlacementIssue::UnbuildableTerrain);
        }
        for (obstacle_index, obstacle) in obstacles.iter().enumerate() {
            if intersects(geometry.footprint, *obstacle) {
                placement.issues.push(PlacementIssue::OverlapsObstacle {
                    index: obstacle_index,
                });
            }
        }
        // Every pair is recorded on both placements so each proposal is independently useful.
        for (other_index, other) in geometries.iter().enumerate() {
            if other_index != index && intersects(geometry.footprint, other.footprint) {
                placement
                    .issues
                    .push(PlacementIssue::OverlapsBuilding { index: other_index });
            }
        }
    }
}

fn rect_in_map(rect: PixelRect, map_width: i64, map_height: i64) -> bool {
    i64::from(rect.left) >= 0
        && i64::from(rect.top) >= 0
        && i64::from(rect.right) <= map_width
        && i64::from(rect.bottom) <= map_height
}

fn footprint_buildable(grid: &TerrainGrid, footprint: PixelRect) -> bool {
    let left = usize::try_from(footprint.left / 8).expect("in-map footprint");
    let top = usize::try_from(footprint.top / 8).expect("in-map footprint");
    let right = usize::try_from(footprint.right / 8).expect("tile-aligned footprint");
    let bottom = usize::try_from(footprint.bottom / 8).expect("tile-aligned footprint");
    let width = grid.width() as usize;
    for y in top..bottom {
        for x in left..right {
            let cell = &grid.cells()[y * width + x];
            if !cell.walkable || !cell.terrain_buildable {
                return false;
            }
        }
    }
    true
}

fn intersects(a: PixelRect, b: PixelRect) -> bool {
    a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom
}

struct DifferenceGrid {
    width: usize,
    height: usize,
    values: Vec<i32>,
}

impl DifferenceGrid {
    fn new(width: usize, height: usize) -> Self {
        let stride = width + 1;
        let len = stride
            .checked_mul(height + 1)
            .expect("validated center cap bounds difference grid");
        Self {
            width,
            height,
            values: vec![0; len],
        }
    }

    fn add_rect(&mut self, left: usize, top: usize, right: usize, bottom: usize) {
        if left >= right || top >= bottom {
            return;
        }
        let stride = self.width + 1;
        self.values[top * stride + left] += 1;
        self.values[top * stride + right] -= 1;
        self.values[bottom * stride + left] -= 1;
        self.values[bottom * stride + right] += 1;
    }

    fn finish(mut self) -> Vec<i32> {
        let stride = self.width + 1;
        for y in 0..=self.height {
            for x in 0..=self.width {
                let index = y * stride + x;
                let from_left = if x > 0 { self.values[index - 1] } else { 0 };
                let from_top = if y > 0 {
                    self.values[index - stride]
                } else {
                    0
                };
                let from_corner = if x > 0 && y > 0 {
                    self.values[index - stride - 1]
                } else {
                    0
                };
                self.values[index] += from_left + from_top - from_corner;
            }
        }
        let mut counts = Vec::with_capacity(self.width * self.height);
        for y in 0..self.height {
            let start = y * stride;
            counts.extend_from_slice(&self.values[start..start + self.width]);
        }
        counts
    }
}

fn stamp_terrain_blockers(
    grid: &TerrainGrid,
    window: PixelRect,
    mover: CollisionBounds,
    difference: &mut DifferenceGrid,
) {
    let width = grid.width() as usize;
    for (index, cell) in grid.cells().iter().enumerate() {
        if cell.walkable {
            continue;
        }
        let x = (index % width) as i64 * PIXELS_PER_WALK_CELL;
        let y = (index / width) as i64 * PIXELS_PER_WALK_CELL;
        stamp_expanded_i64(
            x,
            y,
            x + PIXELS_PER_WALK_CELL,
            y + PIXELS_PER_WALK_CELL,
            window,
            mover,
            difference,
        );
    }
}

fn stamp_expanded(
    blocker: PixelRect,
    window: PixelRect,
    mover: CollisionBounds,
    difference: &mut DifferenceGrid,
) {
    stamp_expanded_i64(
        i64::from(blocker.left),
        i64::from(blocker.top),
        i64::from(blocker.right),
        i64::from(blocker.bottom),
        window,
        mover,
        difference,
    );
}

fn stamp_expanded_i64(
    left: i64,
    top: i64,
    right: i64,
    bottom: i64,
    window: PixelRect,
    mover: CollisionBounds,
    difference: &mut DifferenceGrid,
) {
    // A body [x-left, x+right+1) intersects [blocker.left, blocker.right) exactly when
    // x is in [blocker.left-mover.right, blocker.right+mover.left).
    let expanded_left = left - i64::from(mover.right);
    let expanded_top = top - i64::from(mover.bottom);
    let expanded_right = right + i64::from(mover.left);
    let expanded_bottom = bottom + i64::from(mover.top);
    let clip_left = expanded_left.max(i64::from(window.left));
    let clip_top = expanded_top.max(i64::from(window.top));
    let clip_right = expanded_right.min(i64::from(window.right));
    let clip_bottom = expanded_bottom.min(i64::from(window.bottom));
    if clip_left >= clip_right || clip_top >= clip_bottom {
        return;
    }
    difference.add_rect(
        usize::try_from(clip_left - i64::from(window.left)).expect("clipped nonnegative x"),
        usize::try_from(clip_top - i64::from(window.top)).expect("clipped nonnegative y"),
        usize::try_from(clip_right - i64::from(window.left)).expect("clipped nonnegative x"),
        usize::try_from(clip_bottom - i64::from(window.top)).expect("clipped nonnegative y"),
    );
}

#[allow(clippy::too_many_arguments)]
fn blocked_centers(
    base_counts: &[i32],
    width: usize,
    height: usize,
    window: PixelRect,
    map_width: i64,
    map_height: i64,
    mover: CollisionBounds,
    plan_counts: Option<&[i32]>,
) -> Vec<bool> {
    let mut result = vec![false; width * height];
    for y in 0..height {
        let center_y = i64::from(window.top) + y as i64;
        for x in 0..width {
            let center_x = i64::from(window.left) + x as i64;
            let index = y * width + x;
            result[index] = base_counts[index] > 0
                || plan_counts.is_some_and(|counts| counts[index] > 0)
                || center_x - i64::from(mover.left) < 0
                || center_y - i64::from(mover.top) < 0
                || center_x + i64::from(mover.right) >= map_width
                || center_y + i64::from(mover.bottom) >= map_height;
        }
    }
    result
}

fn local_index(position: PixelPosition, window: PixelRect, width: usize) -> usize {
    let x = usize::try_from(i64::from(position.x) - i64::from(window.left))
        .expect("validated endpoint x");
    let y = usize::try_from(i64::from(position.y) - i64::from(window.top))
        .expect("validated endpoint y");
    y * width + x
}

fn route(
    blocked: &[bool],
    width: usize,
    height: usize,
    start: usize,
    end: usize,
) -> Option<Vec<usize>> {
    if blocked[start] || blocked[end] {
        return None;
    }
    let mut previous = vec![usize::MAX; blocked.len()];
    let mut queue = VecDeque::new();
    previous[start] = start;
    queue.push_back(start);
    // Fixed order makes equivalent shortest routes deterministic.
    const STEPS: [(isize, isize); 8] = [
        (1, 0),
        (0, 1),
        (-1, 0),
        (0, -1),
        (1, 1),
        (-1, 1),
        (-1, -1),
        (1, -1),
    ];
    while let Some(current) = queue.pop_front() {
        if current == end {
            let mut result = vec![end];
            let mut cursor = end;
            while cursor != start {
                cursor = previous[cursor];
                result.push(cursor);
            }
            result.reverse();
            return Some(result);
        }
        let x = current % width;
        let y = current / width;
        for (dx, dy) in STEPS {
            let next_x = x as isize + dx;
            let next_y = y as isize + dy;
            if next_x < 0 || next_y < 0 || next_x >= width as isize || next_y >= height as isize {
                continue;
            }
            let next_x = next_x as usize;
            let next_y = next_y as usize;
            let next = next_y * width + next_x;
            if blocked[next] || previous[next] != usize::MAX {
                continue;
            }
            if dx != 0 && dy != 0 {
                let side_a = y * width + next_x;
                let side_b = next_y * width + x;
                if blocked[side_a] || blocked[side_b] {
                    continue;
                }
            }
            previous[next] = current;
            queue.push_back(next);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerrainCell;

    fn terrain(width: u32, height: u32, blocked: impl Fn(u32, u32) -> bool + Copy) -> TerrainGrid {
        TerrainGrid::from_cells(
            width,
            height,
            (0..height)
                .flat_map(|y| {
                    (0..width).map(move |x| TerrainCell {
                        walkable: !blocked(x, y),
                        terrain_buildable: true,
                        elevation: 0,
                        ramp: false,
                    })
                })
                .collect(),
        )
        .unwrap()
    }

    fn rect(left: i32, top: i32, right: i32, bottom: i32) -> PixelRect {
        PixelRect {
            left,
            top,
            right,
            bottom,
        }
    }

    fn query(window: PixelRect, start: (u32, u32), end: (u32, u32)) -> WallQuery {
        WallQuery {
            window,
            start: PixelPosition {
                x: start.0,
                y: start.1,
            },
            end: PixelPosition { x: end.0, y: end.1 },
            mover: CollisionBounds {
                left: 0,
                top: 0,
                right: 0,
                bottom: 0,
            },
            buildings: Vec::new(),
        }
    }

    fn point_blocked(
        grid: &TerrainGrid,
        obstacles: &[PixelRect],
        collisions: &[PixelRect],
        mover: CollisionBounds,
        point: PixelPosition,
    ) -> bool {
        let body = rect(
            point.x as i32 - mover.left as i32,
            point.y as i32 - mover.top as i32,
            point.x as i32 + mover.right as i32 + 1,
            point.y as i32 + mover.bottom as i32 + 1,
        );
        let map = rect(0, 0, (grid.width() * 8) as i32, (grid.height() * 8) as i32);
        if body.left < map.left
            || body.top < map.top
            || body.right > map.right
            || body.bottom > map.bottom
        {
            return true;
        }
        if obstacles
            .iter()
            .chain(collisions)
            .any(|&blocker| intersects(body, blocker))
        {
            return true;
        }
        grid.cells().iter().enumerate().any(|(index, cell)| {
            !cell.walkable
                && intersects(
                    body,
                    rect(
                        ((index as u32 % grid.width()) * 8) as i32,
                        ((index as u32 / grid.width()) * 8) as i32,
                        ((index as u32 % grid.width() + 1) * 8) as i32,
                        ((index as u32 / grid.width() + 1) * 8) as i32,
                    ),
                )
        })
    }

    // Deliberately separate from the production rectangle difference and index BFS.
    fn brute_route(
        grid: &TerrainGrid,
        obstacles: &[PixelRect],
        collisions: &[PixelRect],
        request: &WallQuery,
    ) -> Option<Vec<PixelPosition>> {
        let width = (request.window.right - request.window.left) as usize;
        let height = (request.window.bottom - request.window.top) as usize;
        let at = |x: usize, y: usize| PixelPosition {
            x: (request.window.left as usize + x) as u32,
            y: (request.window.top as usize + y) as u32,
        };
        let blocked = |x: usize, y: usize| {
            point_blocked(grid, obstacles, collisions, request.mover, at(x, y))
        };
        let sx = (request.start.x as i32 - request.window.left) as usize;
        let sy = (request.start.y as i32 - request.window.top) as usize;
        let ex = (request.end.x as i32 - request.window.left) as usize;
        let ey = (request.end.y as i32 - request.window.top) as usize;
        if blocked(sx, sy) || blocked(ex, ey) {
            return None;
        }
        let mut previous = vec![None; width * height];
        let mut queue = VecDeque::from([(sx, sy)]);
        previous[sy * width + sx] = Some((sx, sy));
        const STEPS: [(isize, isize); 8] = [
            (1, 0),
            (0, 1),
            (-1, 0),
            (0, -1),
            (1, 1),
            (-1, 1),
            (-1, -1),
            (1, -1),
        ];
        while let Some((x, y)) = queue.pop_front() {
            if (x, y) == (ex, ey) {
                let mut path = vec![at(ex, ey)];
                let mut cursor = (ex, ey);
                while cursor != (sx, sy) {
                    cursor = previous[cursor.1 * width + cursor.0].unwrap();
                    path.push(at(cursor.0, cursor.1));
                }
                path.reverse();
                return Some(path);
            }
            for (dx, dy) in STEPS {
                let nx = x as isize + dx;
                let ny = y as isize + dy;
                if nx < 0 || ny < 0 || nx >= width as isize || ny >= height as isize {
                    continue;
                }
                let (nx, ny) = (nx as usize, ny as usize);
                if previous[ny * width + nx].is_some() || blocked(nx, ny) {
                    continue;
                }
                if dx != 0 && dy != 0 && (blocked(nx, y) || blocked(x, ny)) {
                    continue;
                }
                previous[ny * width + nx] = Some((x, y));
                queue.push_back((nx, ny));
            }
        }
        None
    }

    #[test]
    fn asymmetric_expansion_has_exact_inclusive_one_pixel_edges() {
        let mut difference = DifferenceGrid::new(32, 1);
        stamp_expanded(
            rect(20, 0, 21, 1),
            rect(0, 0, 32, 1),
            CollisionBounds {
                left: 2,
                top: 0,
                right: 5,
                bottom: 0,
            },
            &mut difference,
        );
        let counts = difference.finish();
        for (x, count) in counts.iter().enumerate() {
            assert_eq!(*count > 0, (15..23).contains(&x), "unexpected x={x}");
        }
    }

    #[test]
    fn exact_gap_width_accepts_the_body_but_one_more_pixel_does_not() {
        let grid = terrain(8, 8, |_, _| false);
        let obstacles = [rect(0, 0, 24, 64), rect(40, 0, 64, 64)];
        let mut request = query(rect(0, 0, 64, 64), (31, 8), (31, 55));
        request.mover = CollisionBounds {
            left: 7,
            top: 3,
            right: 8,
            bottom: 4,
        };
        assert_eq!(
            check_wall(&grid, &obstacles, &request).unwrap().passage,
            WallPassage::Open
        );
        request.mover.right += 1;
        assert_eq!(
            check_wall(&grid, &obstacles, &request).unwrap().passage,
            WallPassage::StartBlocked
        );
    }

    #[test]
    fn diagonal_cannot_cut_a_blocked_corner() {
        assert_eq!(route(&[false, true, true, false], 2, 2, 0, 3), None);
    }

    #[test]
    fn off_window_terrain_and_map_edges_still_block_the_mover_body() {
        let grid = terrain(4, 2, |x, _| x == 0);
        let mut request = query(rect(8, 0, 16, 8), (8, 1), (15, 1));
        request.mover.left = 3;
        let check = check_wall(&grid, &[], &request).unwrap();
        assert_eq!(check.passage, WallPassage::StartBlocked);

        let clear = terrain(4, 4, |_, _| false);
        let mut edge = query(rect(0, 0, 32, 32), (0, 10), (10, 10));
        edge.mover.left = 1;
        assert_eq!(
            check_wall(&clear, &[], &edge).unwrap().passage,
            WallPassage::StartBlocked
        );
    }

    #[test]
    fn placement_uses_footprint_but_passage_uses_collision_even_when_invalid() {
        let grid = TerrainGrid::from_cells(
            8,
            8,
            (0..64)
                .map(|index| TerrainCell {
                    walkable: index != 7,
                    terrain_buildable: index != 7,
                    elevation: 0,
                    ramp: false,
                })
                .collect(),
        )
        .unwrap();
        let mut request = query(rect(0, 0, 64, 64), (2, 2), (60, 48));
        request.buildings.push(WallBuilding {
            tile: TilePosition { x: 1, y: 1 },
            footprint: DepotFootprint {
                width_tiles: 1,
                height_tiles: 1,
            },
            // The footprint is valid; the collision reaches outside map bounds and covers end.
            collision: CollisionBounds {
                left: 0,
                top: 0,
                right: 20,
                bottom: 0,
            },
        });
        let check = check_wall(&grid, &[], &request).unwrap();
        assert!(
            check.placements[0]
                .issues
                .contains(&PlacementIssue::OutOfMap)
        );
        assert!(
            !check.placements[0]
                .issues
                .contains(&PlacementIssue::UnbuildableTerrain)
        );
        assert_eq!(check.passage, WallPassage::EndBlocked);
    }

    #[test]
    fn baseline_disconnection_and_hostile_profiles_are_explicit() {
        let grid = terrain(4, 4, |x, _| x == 1);
        let request = query(rect(0, 0, 32, 32), (2, 2), (20, 2));
        let check = check_wall(&grid, &[], &request).unwrap();
        assert!(!check.baseline_passable);
        assert_eq!(check.passage, WallPassage::AlreadyDisconnected);

        let clear = terrain(4, 4, |_, _| false);
        let mut hostile = query(rect(0, 0, 32, 32), (1, 1), (30, 30));
        hostile.mover.right = u32::MAX;
        assert_eq!(
            check_wall(&clear, &[], &hostile),
            Err(WallError::MoverExtentTooLarge)
        );
        hostile.mover.right = 0;
        hostile.buildings.push(WallBuilding {
            tile: TilePosition { x: u32::MAX, y: 0 },
            footprint: DepotFootprint {
                width_tiles: 1,
                height_tiles: 1,
            },
            collision: CollisionBounds {
                left: 0,
                top: 0,
                right: 0,
                bottom: 0,
            },
        });
        assert!(matches!(
            check_wall(&clear, &[], &hostile),
            Err(WallError::CoordinateOverflow { .. })
        ));
    }

    #[test]
    fn randomized_small_queries_match_independent_geometry_and_bfs_oracle() {
        let mut state = 0x907c_221d_u64;
        for _ in 0..96 {
            let next = |state: &mut u64| {
                *state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
                *state
            };
            let bits = next(&mut state) & next(&mut state) & next(&mut state);
            let grid = terrain(4, 4, |x, y| bits & (1 << (y * 4 + x)) != 0);
            let mut request = query(rect(3, 3, 29, 29), (5, 5), (26, 26));
            request.mover = CollisionBounds {
                left: (next(&mut state) % 3) as u32,
                top: (next(&mut state) % 3) as u32,
                right: (next(&mut state) % 3) as u32,
                bottom: (next(&mut state) % 3) as u32,
            };
            let obstacle = rect(
                (next(&mut state) % 25) as i32 - 4,
                (next(&mut state) % 25) as i32 - 4,
                (next(&mut state) % 25) as i32 + 4,
                (next(&mut state) % 25) as i32 + 4,
            );
            let obstacle = rect(
                obstacle.left.min(obstacle.right - 1),
                obstacle.top.min(obstacle.bottom - 1),
                obstacle.left.max(obstacle.right - 1) + 1,
                obstacle.top.max(obstacle.bottom - 1) + 1,
            );
            if next(&mut state) & 2 != 0 {
                request.buildings.push(WallBuilding {
                    tile: TilePosition { x: 0, y: 0 },
                    footprint: DepotFootprint {
                        width_tiles: 1,
                        height_tiles: 1,
                    },
                    collision: CollisionBounds {
                        left: (next(&mut state) % 16) as u32,
                        top: (next(&mut state) % 16) as u32,
                        right: (next(&mut state) % 16) as u32,
                        bottom: (next(&mut state) % 16) as u32,
                    },
                });
            }
            let check = check_wall(&grid, &[obstacle], &request).unwrap();
            let collisions: Vec<_> = check.placements.iter().map(|p| p.collision).collect();
            let baseline = brute_route(&grid, &[obstacle], &[], &request);
            let post = brute_route(&grid, &[obstacle], &collisions, &request);
            let start_blocked = point_blocked(
                &grid,
                &[obstacle],
                &collisions,
                request.mover,
                request.start,
            );
            let end_blocked =
                point_blocked(&grid, &[obstacle], &collisions, request.mover, request.end);
            let expected = if start_blocked {
                WallPassage::StartBlocked
            } else if end_blocked {
                WallPassage::EndBlocked
            } else if post.is_some() {
                WallPassage::Open
            } else if baseline.is_none() {
                WallPassage::AlreadyDisconnected
            } else {
                WallPassage::BlockedWithinWindow
            };
            assert_eq!(check.baseline_passable, baseline.is_some());
            assert_eq!(check.passage, expected);
            if expected == WallPassage::Open {
                assert_eq!(check.route, post.unwrap());
            } else {
                assert!(check.route.is_empty());
            }
        }
    }
}
