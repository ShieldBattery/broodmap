//! Incremental, bounded automatic search for static `pixel-rect-v1` wall layouts.
//!
//! The search uses the placement and passage rules from the parent module. Terrain and obstacle
//! blockers are rasterized once for each requested mover; a state only stamps selected collisions.

use std::collections::{HashMap, HashSet, VecDeque};

use thiserror::Error;

use super::{
    CollisionBounds, DifferenceGrid, WallBuilding, WallEndpoint, WallError, WallQuery,
    assess_placements, blocked_centers, intersects, local_index, route, stamp_expanded,
    stamp_terrain_blockers, validate_buildings, validate_query,
};
use crate::{DepotFootprint, PixelPosition, PixelRect, TerrainGrid, TilePosition, WalkPosition};

const MAX_SEARCH_BUILDINGS: usize = 6;
const MAX_SEARCH_STATES: u32 = 100_000;
const MAX_SEARCH_RESULTS: usize = 32;
const MAX_CANDIDATE_PLACEMENTS: usize = 6_144;
const MAX_RETURNED_ALLOW_ROUTE_POINTS: usize = 1_000_000;
const MAX_PLACEMENT_SITE_DISTANCE_PIXELS: u32 = 1_024;

/// A building type available to the finite wall-search inventory.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BuildingSpec {
    pub footprint: DepotFootprint,
    pub collision: CollisionBounds,
}

/// A finite entrance segment around which automatic wall placements are considered.
///
/// A candidate's footprint center must be within `max_distance_pixels` of the finite segment.
/// The whole footprint must still be legal inside [`WallSearchQuery::placement_area`] and
/// [`WallSearchQuery::window`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct WallPlacementSite {
    pub entrance: [PixelPosition; 2],
    pub max_distance_pixels: u32,
}

/// The locally connected terrain cells eligible as centers for an entrance-relative wall search.
///
/// `cells` is row-major over the walk-cell rectangle beginning at `origin`. A true cell marks
/// walkable terrain connected to the open entrance segment without leaving the site's finite
/// radius neighborhood or the exact search window. It is intentionally independent of building
/// footprints, buildability, elevation, and caller-supplied obstacles; those are checked when a
/// concrete candidate is considered.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallPlacementReach {
    pub origin: WalkPosition,
    pub width: u32,
    pub height: u32,
    pub cells: Vec<bool>,
    window: PixelRect,
}

impl WallPlacementReach {
    /// Returns whether the half-open walk cell containing `position` is locally reachable.
    /// Points outside the exact pixel search window are always rejected, including points in a
    /// partially covered edge walk cell.
    pub fn contains_pixel(&self, position: PixelPosition) -> bool {
        let x = i64::from(position.x);
        let y = i64::from(position.y);
        if x < i64::from(self.window.left)
            || x >= i64::from(self.window.right)
            || y < i64::from(self.window.top)
            || y >= i64::from(self.window.bottom)
        {
            return false;
        }
        let cell_x = position.x / 8;
        let cell_y = position.y / 8;
        if cell_x < self.origin.x
            || cell_y < self.origin.y
            || cell_x >= self.origin.x + self.width
            || cell_y >= self.origin.y + self.height
        {
            return false;
        }
        let index = (cell_y - self.origin.y) as usize * self.width as usize
            + (cell_x - self.origin.x) as usize;
        self.cells.get(index).copied().unwrap_or(false)
    }
}

/// Computes the terrain-connected part of a finite placement site's Euclidean neighborhood.
///
/// The finite radius remains a geometric center constraint for candidates. This mask adds a
/// local terrain-connectivity constraint: it flood-fills walkable 8x8 cells whose clipped cell
/// rectangle intersects the site's capsule. Seeds are cells with positive-length overlap with
/// the *open* entrance segment, so endpoint and corner-only contacts do not attach an unrelated
/// ledge. Diagonal flood steps require both cardinal side cells to be admissible, matching the
/// terrain route model's no-corner-cutting rule.
pub fn wall_placement_reach(
    grid: &TerrainGrid,
    window: PixelRect,
    site: WallPlacementSite,
) -> Result<WallPlacementReach, SearchError> {
    let map_width = i64::from(grid.width()) * super::PIXELS_PER_WALK_CELL;
    let map_height = i64::from(grid.height()) * super::PIXELS_PER_WALK_CELL;
    // Reuse wall-query validation for the public window bounds and 1024-pixel cap. The anchor
    // is inside every valid window; invalid windows fail before endpoint validation.
    let anchor = PixelPosition {
        x: window.left.max(0) as u32,
        y: window.top.max(0) as u32,
    };
    let window = validate_query(
        map_width,
        map_height,
        &[],
        &WallQuery {
            window,
            start: anchor,
            end: anchor,
            mover: CollisionBounds {
                left: 0,
                top: 0,
                right: 0,
                bottom: 0,
            },
            buildings: Vec::new(),
        },
    )?;
    validate_placement_site(site, map_width, map_height)?;

    let origin = WalkPosition {
        x: (window.left / 8) as u32,
        y: (window.top / 8) as u32,
    };
    let right = ((i64::from(window.right) + 7) / 8) as u32;
    let bottom = ((i64::from(window.bottom) + 7) / 8) as u32;
    let width = right - origin.x;
    let height = bottom - origin.y;
    let len = width as usize * height as usize;
    let mut admissible = vec![false; len];
    let mut seeds = Vec::new();
    for local_y in 0..height {
        for local_x in 0..width {
            let cell_x = origin.x + local_x;
            let cell_y = origin.y + local_y;
            let index = local_y as usize * width as usize + local_x as usize;
            let cell = &grid.cells()[cell_y as usize * grid.width() as usize + cell_x as usize];
            let cell_rect = clipped_cell_rect(cell_x, cell_y, window);
            if !cell.walkable || !cell_rect_intersects_capsule(cell_rect, site) {
                continue;
            }
            admissible[index] = true;
            if open_segment_overlaps_rect(site.entrance, cell_rect) {
                seeds.push(index);
            }
        }
    }

    let mut cells = vec![false; len];
    let mut queue = VecDeque::new();
    for seed in seeds {
        if !cells[seed] {
            cells[seed] = true;
            queue.push_back(seed);
        }
    }
    while let Some(index) = queue.pop_front() {
        let x = index % width as usize;
        let y = index / width as usize;
        for (dx, dy) in [
            (-1_i32, -1_i32),
            (0, -1),
            (1, -1),
            (-1, 0),
            (1, 0),
            (-1, 1),
            (0, 1),
            (1, 1),
        ] {
            let next_x = x as i32 + dx;
            let next_y = y as i32 + dy;
            if next_x < 0 || next_y < 0 || next_x >= width as i32 || next_y >= height as i32 {
                continue;
            }
            let next = next_y as usize * width as usize + next_x as usize;
            if !admissible[next] || cells[next] {
                continue;
            }
            if dx != 0 && dy != 0 {
                let side_a = y * width as usize + next_x as usize;
                let side_b = next_y as usize * width as usize + x;
                if !admissible[side_a] || !admissible[side_b] {
                    continue;
                }
            }
            cells[next] = true;
            queue.push_back(next);
        }
    }
    Ok(WallPlacementReach {
        origin,
        width,
        height,
        cells,
        window,
    })
}

fn clipped_cell_rect(cell_x: u32, cell_y: u32, window: PixelRect) -> PixelRect {
    let left = i64::from(cell_x) * 8;
    let top = i64::from(cell_y) * 8;
    PixelRect {
        left: i32::try_from(left.max(i64::from(window.left))).expect("clipped cell left fits i32"),
        top: i32::try_from(top.max(i64::from(window.top))).expect("clipped cell top fits i32"),
        right: i32::try_from((left + 8).min(i64::from(window.right)))
            .expect("clipped cell right fits i32"),
        bottom: i32::try_from((top + 8).min(i64::from(window.bottom)))
            .expect("clipped cell bottom fits i32"),
    }
}

fn cell_rect_intersects_capsule(rect: PixelRect, site: WallPlacementSite) -> bool {
    if segment_rect_interval(site.entrance, rect).is_some() {
        return true;
    }
    let radius_squared = i64::from(site.max_distance_pixels).pow(2);
    site.entrance.into_iter().any(|endpoint| {
        let x = i64::from(endpoint.x).clamp(i64::from(rect.left), i64::from(rect.right));
        let y = i64::from(endpoint.y).clamp(i64::from(rect.top), i64::from(rect.bottom));
        let dx = i64::from(endpoint.x) - x;
        let dy = i64::from(endpoint.y) - y;
        dx * dx + dy * dy <= radius_squared
    }) || [
        (i64::from(rect.left), i64::from(rect.top)),
        (i64::from(rect.right), i64::from(rect.top)),
        (i64::from(rect.left), i64::from(rect.bottom)),
        (i64::from(rect.right), i64::from(rect.bottom)),
    ]
    .into_iter()
    .any(|(x, y)| point_in_placement_site(x, y, site))
}

/// Returns the closed interval of the finite segment inside `rect`, if any. This is shared by
/// the conservative capsule raster and the stricter seed predicate so finite endpoints cannot
/// grow an accidental collinear strip beyond the segment.
fn segment_rect_interval(entrance: [PixelPosition; 2], rect: PixelRect) -> Option<(f64, f64)> {
    let start_x = f64::from(entrance[0].x);
    let start_y = f64::from(entrance[0].y);
    let dx = f64::from(entrance[1].x) - start_x;
    let dy = f64::from(entrance[1].y) - start_y;
    let mut low: f64 = 0.0;
    let mut high: f64 = 1.0;
    for (p, q) in [
        (-dx, start_x - f64::from(rect.left)),
        (dx, f64::from(rect.right) - start_x),
        (-dy, start_y - f64::from(rect.top)),
        (dy, f64::from(rect.bottom) - start_y),
    ] {
        if p == 0.0 {
            if q < 0.0 {
                return None;
            }
            continue;
        }
        let t = q / p;
        if p < 0.0 {
            low = low.max(t);
        } else {
            high = high.min(t);
        }
        if low > high {
            return None;
        }
    }
    Some((low, high))
}

fn open_segment_overlaps_rect(entrance: [PixelPosition; 2], rect: PixelRect) -> bool {
    let Some((low, high)) = segment_rect_interval(entrance, rect) else {
        return false;
    };
    // A positive interval within the open segment excludes endpoint-only and corner-only contact,
    // while an edge-aligned segment still seeds both adjacent cells.
    low.max(0.0) < high.min(1.0)
}
/// A bounded automatic wall-search request.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallSearchQuery {
    pub window: PixelRect,
    /// Full proposed footprints must be contained in this rectangle and in `window`.
    pub placement_area: PixelRect,
    /// When set, every walk cell covered by a candidate footprint must have this terrain height.
    pub placement_elevation: Option<u8>,
    /// When set, limit candidate footprint centers to a finite entrance segment and radius.
    /// `None` preserves the full `placement_area` behavior.
    pub placement_site: Option<WallPlacementSite>,
    pub start: PixelPosition,
    pub end: PixelPosition,
    /// The mover whose route each returned layout blocks.
    pub block: CollisionBounds,
    /// An optional mover whose route every returned layout preserves.
    pub allow: Option<CollisionBounds>,
    /// Finite building inventory. Equal specifications are interchangeable.
    pub buildings: Vec<BuildingSpec>,
    /// Total layout checks across all calls to [`WallSearch::step`].
    pub max_states: u32,
    pub max_results: usize,
}

/// Why a search stopped changing.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum WallSearchReason {
    Running,
    Exhausted,
    StateLimit,
    /// The requested layout count was reached, or retaining another exact `allow_route` would
    /// exceed the search-wide route-evidence cap.
    ResultLimit,
}

/// A chosen building and its representative original inventory slot.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SearchPlacement {
    pub spec_index: usize,
    pub building: WallBuilding,
}

/// One minimal static wall layout. `allow_route` is empty if no allowed mover was requested.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallLayout {
    pub buildings: Vec<SearchPlacement>,
    pub allow_route: Vec<PixelPosition>,
}

/// Current progress and accumulated deterministic layouts.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WallSearchResult {
    pub reason: WallSearchReason,
    pub explored_states: u32,
    pub candidate_placements: usize,
    pub layouts: Vec<WallLayout>,
}

/// Construction errors for [`WallSearch`].
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum SearchError {
    #[error(transparent)]
    Wall(#[from] WallError),
    #[error("placement area must be nonempty, inside the map, and inside the search window")]
    InvalidPlacementArea,
    #[error("placement elevation must be 0, 1, or 2, got {actual}")]
    InvalidPlacementElevation { actual: u8 },
    #[error(
        "placement-site max distance must be at most {MAX_PLACEMENT_SITE_DISTANCE_PIXELS} pixels, got {actual}"
    )]
    InvalidPlacementSiteDistance { actual: u32 },
    #[error("wall search has {actual} building slots; the cap is {MAX_SEARCH_BUILDINGS}")]
    TooManyBuildingSlots { actual: usize },
    #[error("wall search max_states must be between 1 and {MAX_SEARCH_STATES}, got {actual}")]
    InvalidStateLimit { actual: u32 },
    #[error("wall search max_results must be between 1 and {MAX_SEARCH_RESULTS}, got {actual}")]
    InvalidResultLimit { actual: usize },
    #[error(
        "wall search generated {actual} candidate placements; the cap is {MAX_CANDIDATE_PLACEMENTS}"
    )]
    TooManyCandidatePlacements { actual: usize },
    #[error("the {mover:?} mover has a blocked {endpoint:?} endpoint before buildings are placed")]
    BaselineEndpointBlocked {
        mover: SearchMover,
        endpoint: WallEndpoint,
    },
    #[error("the {mover:?} mover endpoints are disconnected before buildings are placed")]
    BaselineDisconnected { mover: SearchMover },
    #[error("entrance {endpoint:?} endpoint ({x}, {y}) is outside the map")]
    EntranceOutsideMap {
        endpoint: WallEndpoint,
        x: u32,
        y: u32,
    },
    #[error("entrance endpoints must be distinct")]
    DegenerateEntrance,
    #[error("no connected safe walk-cell-center endpoint pair found outside the construction area")]
    NoSafeEndpointPair,
}

/// Which requested mover failed baseline validation.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SearchMover {
    Block,
    Allow,
}

#[derive(Debug, Clone)]
struct PreparedMover {
    bounds: CollisionBounds,
    baseline: Vec<bool>,
}

#[derive(Debug, Clone)]
struct Candidate {
    spec_index: usize,
    spec_group: usize,
    building: WallBuilding,
    footprint: PixelRect,
    collision: PixelRect,
    site_distance_rank: u128,
}

#[derive(Debug, Clone)]
struct State {
    candidates: Vec<usize>,
    used_groups: Vec<u8>,
}

#[derive(Debug, Clone)]
enum SearchFrame {
    Evaluate(State),
    Extend {
        state: State,
        candidates: Vec<usize>,
        next: usize,
    },
}

/// A resumable, deterministic, route-guided wall search.
#[derive(Debug)]
pub struct WallSearch {
    query: WallSearchQuery,
    width: usize,
    height: usize,
    block: PreparedMover,
    allow: Option<PreparedMover>,
    candidates: Vec<Candidate>,
    group_counts: Vec<u8>,
    stack: Vec<SearchFrame>,
    // Keys enter here only after a state is actually evaluated, so it is bounded by max_states.
    visited: HashSet<Vec<usize>>,
    result: WallSearchResult,
}

impl WallSearch {
    /// Prepares terrain baselines and legal individual placements without evaluating a layout.
    pub fn new(
        grid: &TerrainGrid,
        obstacles: &[PixelRect],
        query: WallSearchQuery,
    ) -> Result<Self, SearchError> {
        let (_window, map_width, map_height) = validate_search_fields(grid, obstacles, &query)?;
        let empty = WallQuery {
            window: query.window,
            start: query.start,
            end: query.end,
            mover: query.block,
            buildings: Vec::new(),
        };
        let window = validate_query(map_width, map_height, obstacles, &empty)?;
        let block = prepare_mover(
            grid,
            obstacles,
            window,
            query.start,
            query.end,
            query.block,
            SearchMover::Block,
        )?;
        let allow = query
            .allow
            .map(|bounds| {
                validate_query(
                    map_width,
                    map_height,
                    obstacles,
                    &WallQuery {
                        mover: bounds,
                        ..empty.clone()
                    },
                )?;
                prepare_mover(
                    grid,
                    obstacles,
                    window,
                    query.start,
                    query.end,
                    bounds,
                    SearchMover::Allow,
                )
            })
            .transpose()?;
        let (candidates, group_counts) = enumerate_search_candidates(
            grid,
            obstacles,
            window,
            query.placement_area,
            &query,
            map_width,
            map_height,
        )?;
        let result = WallSearchResult {
            reason: WallSearchReason::Running,
            explored_states: 0,
            candidate_placements: candidates.len(),
            layouts: Vec::new(),
        };
        let root = State {
            candidates: Vec::new(),
            used_groups: vec![0; group_counts.len()],
        };
        let visited = HashSet::new();
        Ok(Self {
            query,
            width: usize::try_from(window.right - window.left).expect("validated window width"),
            height: usize::try_from(window.bottom - window.top).expect("validated window height"),
            block,
            allow,
            candidates,
            group_counts,
            stack: vec![SearchFrame::Evaluate(root)],
            visited,
            result,
        })
    }

    /// Processes no more than `max_states` state checks. `step(0)` is a no-op.
    pub fn step(&mut self, max_states: u32) {
        if max_states == 0 || self.is_done() {
            return;
        }
        let mut remaining = max_states;
        while remaining > 0 && self.result.reason == WallSearchReason::Running {
            if self.result.explored_states >= self.query.max_states {
                self.result.reason = WallSearchReason::StateLimit;
                break;
            }
            let Some(frame) = self.stack.pop() else {
                self.result.reason = WallSearchReason::Exhausted;
                break;
            };
            match frame {
                SearchFrame::Evaluate(state) => {
                    // States can be reached through different insertion orders. Deduplicate only
                    // now, not while queuing, so DFS can descend before it reserves siblings.
                    if !self.visited.insert(state.candidates.clone()) {
                        continue;
                    }
                    remaining -= 1;
                    self.result.explored_states += 1;
                    self.explore(state);
                }
                SearchFrame::Extend {
                    state,
                    candidates,
                    next,
                } => {
                    if next >= candidates.len() {
                        continue;
                    }
                    let index = candidates[next];
                    let mut child = state.clone();
                    child.candidates.push(index);
                    child.candidates.sort_unstable();
                    child.used_groups[self.candidates[index].spec_group] += 1;
                    // Keep only this frame and its immediate child. This is a true DFS and bounds
                    // pending state storage by search depth plus one candidate list per depth.
                    self.stack.push(SearchFrame::Extend {
                        state,
                        candidates,
                        next: next + 1,
                    });
                    self.stack.push(SearchFrame::Evaluate(child));
                }
            }
            if self.result.reason != WallSearchReason::Running {
                break;
            }
            if self.result.layouts.len() >= self.query.max_results {
                self.result.reason = WallSearchReason::ResultLimit;
            } else if self.stack.is_empty() {
                self.result.reason = WallSearchReason::Exhausted;
            } else if self.result.explored_states >= self.query.max_states {
                self.result.reason = WallSearchReason::StateLimit;
            }
        }
    }

    pub fn is_done(&self) -> bool {
        self.result.reason != WallSearchReason::Running
    }

    pub fn result(&self) -> &WallSearchResult {
        &self.result
    }

    fn explore(&mut self, state: State) {
        let start = local_index(self.query.start, self.query.window, self.width);
        let end = local_index(self.query.end, self.query.window, self.width);
        let blocked = self.blocked_for(&self.block, &state.candidates);
        if blocked[start] || blocked[end] {
            return;
        }
        let Some(witness) = route(&blocked, self.width, self.height, start, end) else {
            if !self.is_minimal_block(&state.candidates, start, end) {
                return;
            }
            let allow_route = match &self.allow {
                Some(allow) => {
                    let allowed = self.blocked_for(allow, &state.candidates);
                    if allowed[start] || allowed[end] {
                        return;
                    }
                    let Some(route) = route(&allowed, self.width, self.height, start, end) else {
                        return;
                    };
                    let already_returned: usize = self
                        .result
                        .layouts
                        .iter()
                        .map(|layout| layout.allow_route.len())
                        .sum();
                    if route.len()
                        > MAX_RETURNED_ALLOW_ROUTE_POINTS.saturating_sub(already_returned)
                    {
                        // A returned allow route is exact evidence, never truncated. Reuse the
                        // result limit as the public terminal condition for this bounded output.
                        self.result.reason = WallSearchReason::ResultLimit;
                        return;
                    }
                    route_positions(route, self.query.window, self.width)
                }
                None => Vec::new(),
            };
            self.result.layouts.push(WallLayout {
                buildings: state
                    .candidates
                    .iter()
                    .map(|&index| {
                        let candidate = &self.candidates[index];
                        SearchPlacement {
                            spec_index: candidate.spec_index,
                            building: candidate.building,
                        }
                    })
                    .collect(),
                allow_route,
            });
            return;
        };
        let mut extensions = Vec::new();
        for (index, candidate) in self.candidates.iter().enumerate() {
            if state.candidates.contains(&index)
                || state.used_groups[candidate.spec_group]
                    >= self.group_counts[candidate.spec_group]
                || state
                    .candidates
                    .iter()
                    .any(|&other| intersects(candidate.footprint, self.candidates[other].footprint))
                || !candidate_touches_witness(
                    candidate.collision,
                    self.block.bounds,
                    &witness,
                    self.query.window,
                    self.width,
                )
            {
                continue;
            }
            extensions.push(index);
        }
        if !extensions.is_empty() {
            self.stack.push(SearchFrame::Extend {
                state,
                candidates: extensions,
                next: 0,
            });
        }
    }

    fn is_minimal_block(&self, selected: &[usize], start: usize, end: usize) -> bool {
        selected.iter().enumerate().all(|(remove, _)| {
            let retained: Vec<_> = selected
                .iter()
                .enumerate()
                .filter_map(|(index, &candidate)| (index != remove).then_some(candidate))
                .collect();
            let blocked = self.blocked_for(&self.block, &retained);
            !blocked[start]
                && !blocked[end]
                && route(&blocked, self.width, self.height, start, end).is_some()
        })
    }

    fn blocked_for(&self, mover: &PreparedMover, selected: &[usize]) -> Vec<bool> {
        let mut blocked = mover.baseline.clone();
        for &candidate in selected {
            stamp_collision_into(
                &mut blocked,
                self.query.window,
                self.width,
                self.height,
                self.candidates[candidate].collision,
                mover.bounds,
            );
        }
        blocked
    }
}

fn validate_search_fields(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    query: &WallSearchQuery,
) -> Result<(PixelRect, i64, i64), SearchError> {
    if query.buildings.len() > MAX_SEARCH_BUILDINGS {
        return Err(SearchError::TooManyBuildingSlots {
            actual: query.buildings.len(),
        });
    }
    if query.max_states == 0 || query.max_states > MAX_SEARCH_STATES {
        return Err(SearchError::InvalidStateLimit {
            actual: query.max_states,
        });
    }
    if query.max_results == 0 || query.max_results > MAX_SEARCH_RESULTS {
        return Err(SearchError::InvalidResultLimit {
            actual: query.max_results,
        });
    }
    if let Some(actual) = query.placement_elevation.filter(|height| *height > 2) {
        return Err(SearchError::InvalidPlacementElevation { actual });
    }
    let map_width = i64::from(grid.width()) * super::PIXELS_PER_WALK_CELL;
    let map_height = i64::from(grid.height()) * super::PIXELS_PER_WALK_CELL;
    if let Some(site) = query.placement_site {
        validate_placement_site(site, map_width, map_height)?;
    }
    // This anchor validates all non-endpoint WallQuery fields for automatic suggestions, whose
    // caller-provided start/end are deliberately ignored.
    let anchor = PixelPosition {
        x: query.window.left.max(0) as u32,
        y: query.window.top.max(0) as u32,
    };
    let window = validate_query(
        map_width,
        map_height,
        obstacles,
        &WallQuery {
            window: query.window,
            start: anchor,
            end: anchor,
            mover: query.block,
            buildings: Vec::new(),
        },
    )?;
    if let Some(mover) = query.allow {
        validate_query(
            map_width,
            map_height,
            obstacles,
            &WallQuery {
                window: query.window,
                start: anchor,
                end: anchor,
                mover,
                buildings: Vec::new(),
            },
        )?;
    }
    validate_placement_area(query.placement_area, window, map_width, map_height)?;
    Ok((window, map_width, map_height))
}

fn validate_placement_site(
    site: WallPlacementSite,
    map_width: i64,
    map_height: i64,
) -> Result<(), SearchError> {
    if site.max_distance_pixels > MAX_PLACEMENT_SITE_DISTANCE_PIXELS {
        return Err(SearchError::InvalidPlacementSiteDistance {
            actual: site.max_distance_pixels,
        });
    }
    for (endpoint, position) in [
        (WallEndpoint::Start, site.entrance[0]),
        (WallEndpoint::End, site.entrance[1]),
    ] {
        if i64::from(position.x) > map_width || i64::from(position.y) > map_height {
            return Err(SearchError::EntranceOutsideMap {
                endpoint,
                x: position.x,
                y: position.y,
            });
        }
    }
    if site.entrance[0] == site.entrance[1] {
        return Err(SearchError::DegenerateEntrance);
    }
    Ok(())
}

/// Suggests walk-cell-center endpoints outside every possible selected building collision body.
///
/// The request's `start` and `end` are ignored; `entrance` defines the local opening direction.
/// A lies on the negative side of the entrance normal `(-dy, dx)` and B on its positive side.
/// Both requested movers must connect them before construction. Rectangle mode excludes the full
/// placement area with building overhang and mover clearance. Placement-site mode instead excludes
/// the union of collision bodies for legal candidate placements, for both requested movers.
/// Each mover needs a bounded full-window component pass. This helper samples walk-cell centers:
/// failure does not prove that no suitable integer-pixel pair exists. A larger routing window or
/// smaller placement area can expose more safe candidates.
pub fn suggest_wall_endpoints(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    query: &WallSearchQuery,
    entrance: [PixelPosition; 2],
) -> Result<[PixelPosition; 2], SearchError> {
    let (window, map_width, map_height) = validate_search_fields(grid, obstacles, query)?;
    for (endpoint, position) in [
        (WallEndpoint::Start, entrance[0]),
        (WallEndpoint::End, entrance[1]),
    ] {
        if i64::from(position.x) > map_width || i64::from(position.y) > map_height {
            return Err(SearchError::EntranceOutsideMap {
                endpoint,
                x: position.x,
                y: position.y,
            });
        }
    }
    if entrance[0] == entrance[1] {
        return Err(SearchError::DegenerateEntrance);
    }

    let (rectangle_exclusion, candidate_exclusion) = match query.placement_site {
        Some(_) => {
            let (candidates, _) = enumerate_candidates(
                grid,
                obstacles,
                window,
                query.placement_area,
                query,
                map_width,
                map_height,
            )?;
            (
                None,
                Some(candidate_endpoint_exclusion(window, &candidates, query)),
            )
        }
        None => (Some(endpoint_exclusion(query)?), None),
    };
    let width = usize::try_from(window.right - window.left).expect("validated window width");
    let height = usize::try_from(window.bottom - window.top).expect("validated window height");
    let block_baseline = mover_baseline(grid, obstacles, window, query.block);
    let allow_baseline = query
        .allow
        .map(|mover| mover_baseline(grid, obstacles, window, mover));
    let block_components = component_labels(&block_baseline, width, height);
    let allow_components = allow_baseline
        .as_deref()
        .map(|blocked| component_labels(blocked, width, height));

    let dx = i64::from(entrance[1].x) - i64::from(entrance[0].x);
    let dy = i64::from(entrance[1].y) - i64::from(entrance[0].y);
    let midpoint_x = i64::from(entrance[0].x) + i64::from(entrance[1].x);
    let midpoint_y = i64::from(entrance[0].y) + i64::from(entrance[1].y);
    let mut positive = HashMap::new();
    let mut negative = HashMap::new();
    for cell_y in 0..grid.height() {
        let y = i64::from(cell_y) * super::PIXELS_PER_WALK_CELL + 4;
        if y < i64::from(window.top) || y >= i64::from(window.bottom) {
            continue;
        }
        for cell_x in 0..grid.width() {
            let x = i64::from(cell_x) * super::PIXELS_PER_WALK_CELL + 4;
            if x < i64::from(window.left)
                || x >= i64::from(window.right)
                || rectangle_exclusion.is_some_and(|exclusion| point_in_rect(x, y, exclusion))
                || query
                    .placement_site
                    .is_some_and(|site| point_in_placement_site(x, y, site))
            {
                continue;
            }
            let normal = (2 * x - midpoint_x) as i128 * -(dy as i128)
                + (2 * y - midpoint_y) as i128 * dx as i128;
            if normal == 0 {
                continue;
            }
            let index = (y - i64::from(window.top)) as usize * width
                + (x - i64::from(window.left)) as usize;
            if candidate_exclusion
                .as_ref()
                .is_some_and(|excluded| excluded[index])
            {
                continue;
            }
            let block_component = block_components[index];
            if block_component == usize::MAX {
                continue;
            }
            let allow_component = allow_components
                .as_ref()
                .map_or(usize::MAX, |components| components[index]);
            if allow_components.is_some() && allow_component == usize::MAX {
                continue;
            }
            let lateral = ((2 * x - midpoint_x) as i128 * dx as i128
                + (2 * y - midpoint_y) as i128 * dy as i128)
                .unsigned_abs();
            let candidate = EndpointCandidate {
                position: PixelPosition {
                    x: x as u32,
                    y: y as u32,
                },
                rank: (lateral, normal.unsigned_abs()),
            };
            let slot = if normal > 0 {
                &mut positive
            } else {
                &mut negative
            };
            let key = (block_component, allow_component);
            if slot
                .get(&key)
                .is_none_or(|old: &EndpointCandidate| candidate.better_than(old))
            {
                slot.insert(key, candidate);
            }
        }
    }
    let mut best: Option<(EndpointCandidate, EndpointCandidate)> = None;
    for (key, first) in positive {
        let Some(second) = negative.get(&key) else {
            continue;
        };
        if best
            .as_ref()
            .is_none_or(|old| pair_better(first, *second, old.0, old.1))
        {
            best = Some((first, *second));
        }
    }
    // Preserve the UI convention: A is the negative-normal side and B is the positive-normal side.
    best.map(|(positive, negative)| [negative.position, positive.position])
        .ok_or(SearchError::NoSafeEndpointPair)
}

#[derive(Debug, Copy, Clone)]
struct EndpointCandidate {
    position: PixelPosition,
    // Continue straight out of the entrance first; then choose the nearest point along that ray.
    rank: (u128, u128),
}

impl EndpointCandidate {
    fn better_than(&self, other: &Self) -> bool {
        (self.rank, self.position.y, self.position.x)
            < (other.rank, other.position.y, other.position.x)
    }
}

fn pair_better(
    first: EndpointCandidate,
    second: EndpointCandidate,
    old_first: EndpointCandidate,
    old_second: EndpointCandidate,
) -> bool {
    (
        first.rank.0 + second.rank.0,
        first.rank.1 + second.rank.1,
        first.position.y,
        first.position.x,
        second.position.y,
        second.position.x,
    ) < (
        old_first.rank.0 + old_second.rank.0,
        old_first.rank.1 + old_second.rank.1,
        old_first.position.y,
        old_first.position.x,
        old_second.position.y,
        old_second.position.x,
    )
}

fn endpoint_exclusion(query: &WallSearchQuery) -> Result<(i64, i64, i64, i64), SearchError> {
    let mut outward = (0_i64, 0_i64, 0_i64, 0_i64);
    for spec in &query.buildings {
        let prototype = WallBuilding {
            tile: TilePosition { x: 0, y: 0 },
            footprint: spec.footprint,
            collision: spec.collision,
        };
        let geometry = validate_buildings(&[prototype])?
            .pop()
            .expect("one prototype geometry");
        outward.0 = outward
            .0
            .max(i64::from(geometry.footprint.left - geometry.collision.left));
        outward.1 = outward
            .1
            .max(i64::from(geometry.footprint.top - geometry.collision.top));
        outward.2 = outward.2.max(i64::from(
            geometry.collision.right - geometry.footprint.right,
        ));
        outward.3 = outward.3.max(i64::from(
            geometry.collision.bottom - geometry.footprint.bottom,
        ));
    }
    // A center is blocked when its mover body reaches a possible collision rectangle. Extend the
    // entire placement domain by the largest combined building-overhang and mover extent.
    let mut extension = (0_i64, 0_i64, 0_i64, 0_i64);
    for mover in std::iter::once(query.block).chain(query.allow) {
        extension.0 = extension.0.max(outward.0 + i64::from(mover.right));
        extension.1 = extension.1.max(outward.1 + i64::from(mover.bottom));
        extension.2 = extension.2.max(outward.2 + i64::from(mover.left));
        extension.3 = extension.3.max(outward.3 + i64::from(mover.top));
    }
    Ok((
        i64::from(query.placement_area.left) - extension.0,
        i64::from(query.placement_area.top) - extension.1,
        i64::from(query.placement_area.right) + extension.2,
        i64::from(query.placement_area.bottom) + extension.3,
    ))
}

fn candidate_endpoint_exclusion(
    window: PixelRect,
    candidates: &[Candidate],
    query: &WallSearchQuery,
) -> Vec<bool> {
    let width = usize::try_from(window.right - window.left).expect("validated window width");
    let height = usize::try_from(window.bottom - window.top).expect("validated window height");
    let mut difference = DifferenceGrid::new(width, height);
    for candidate in candidates {
        stamp_expanded(candidate.collision, window, query.block, &mut difference);
        if let Some(allow) = query.allow {
            stamp_expanded(candidate.collision, window, allow, &mut difference);
        }
    }
    difference
        .finish()
        .into_iter()
        .map(|count| count != 0)
        .collect()
}

fn point_in_rect(x: i64, y: i64, rect: (i64, i64, i64, i64)) -> bool {
    x >= rect.0 && x < rect.2 && y >= rect.1 && y < rect.3
}

fn mover_baseline(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    window: PixelRect,
    bounds: CollisionBounds,
) -> Vec<bool> {
    let width = usize::try_from(window.right - window.left).expect("validated window width");
    let height = usize::try_from(window.bottom - window.top).expect("validated window height");
    let mut difference = DifferenceGrid::new(width, height);
    stamp_terrain_blockers(grid, window, bounds, &mut difference);
    for obstacle in obstacles {
        stamp_expanded(*obstacle, window, bounds, &mut difference);
    }
    blocked_centers(
        &difference.finish(),
        width,
        height,
        window,
        i64::from(grid.width()) * super::PIXELS_PER_WALK_CELL,
        i64::from(grid.height()) * super::PIXELS_PER_WALK_CELL,
        bounds,
        None,
    )
}

fn component_labels(blocked: &[bool], width: usize, height: usize) -> Vec<usize> {
    let mut labels = vec![usize::MAX; blocked.len()];
    let mut next_label = 0;
    for start in 0..blocked.len() {
        if blocked[start] || labels[start] != usize::MAX {
            continue;
        }
        labels[start] = next_label;
        let mut queue = VecDeque::from([start]);
        while let Some(current) = queue.pop_front() {
            let x = current % width;
            let y = current / width;
            for (dx, dy) in [
                (1_isize, 0_isize),
                (0, 1),
                (-1, 0),
                (0, -1),
                (1, 1),
                (-1, 1),
                (-1, -1),
                (1, -1),
            ] {
                let next_x = x as isize + dx;
                let next_y = y as isize + dy;
                if next_x < 0 || next_y < 0 || next_x >= width as isize || next_y >= height as isize
                {
                    continue;
                }
                let next_x = next_x as usize;
                let next_y = next_y as usize;
                let next = next_y * width + next_x;
                if blocked[next] || labels[next] != usize::MAX {
                    continue;
                }
                if dx != 0
                    && dy != 0
                    && (blocked[y * width + next_x] || blocked[next_y * width + x])
                {
                    continue;
                }
                labels[next] = next_label;
                queue.push_back(next);
            }
        }
        next_label += 1;
    }
    labels
}
fn validate_placement_area(
    area: PixelRect,
    window: PixelRect,
    map_width: i64,
    map_height: i64,
) -> Result<(), SearchError> {
    if area.left < 0
        || area.top < 0
        || area.left >= area.right
        || area.top >= area.bottom
        || i64::from(area.right) > map_width
        || i64::from(area.bottom) > map_height
        || area.left < window.left
        || area.top < window.top
        || area.right > window.right
        || area.bottom > window.bottom
    {
        return Err(SearchError::InvalidPlacementArea);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn prepare_mover(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    window: PixelRect,
    start: PixelPosition,
    end: PixelPosition,
    bounds: CollisionBounds,
    mover: SearchMover,
) -> Result<PreparedMover, SearchError> {
    let width = usize::try_from(window.right - window.left).expect("validated window width");
    let height = usize::try_from(window.bottom - window.top).expect("validated window height");
    let mut difference = DifferenceGrid::new(width, height);
    stamp_terrain_blockers(grid, window, bounds, &mut difference);
    for obstacle in obstacles {
        stamp_expanded(*obstacle, window, bounds, &mut difference);
    }
    let counts = difference.finish();
    let baseline = blocked_centers(
        &counts,
        width,
        height,
        window,
        i64::from(grid.width()) * super::PIXELS_PER_WALK_CELL,
        i64::from(grid.height()) * super::PIXELS_PER_WALK_CELL,
        bounds,
        None,
    );
    let start = local_index(start, window, width);
    let end = local_index(end, window, width);
    if baseline[start] {
        return Err(SearchError::BaselineEndpointBlocked {
            mover,
            endpoint: WallEndpoint::Start,
        });
    }
    if baseline[end] {
        return Err(SearchError::BaselineEndpointBlocked {
            mover,
            endpoint: WallEndpoint::End,
        });
    }
    if route(&baseline, width, height, start, end).is_none() {
        return Err(SearchError::BaselineDisconnected { mover });
    }
    Ok(PreparedMover { bounds, baseline })
}

fn enumerate_candidates(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    window: PixelRect,
    area: PixelRect,
    query: &WallSearchQuery,
    map_width: i64,
    map_height: i64,
) -> Result<(Vec<Candidate>, Vec<u8>), SearchError> {
    enumerate_candidates_with_mode(
        grid,
        obstacles,
        window,
        area,
        query,
        map_width,
        map_height,
        CandidateEnumerationMode::EndpointSuggestion,
    )
}

fn enumerate_search_candidates(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    window: PixelRect,
    area: PixelRect,
    query: &WallSearchQuery,
    map_width: i64,
    map_height: i64,
) -> Result<(Vec<Candidate>, Vec<u8>), SearchError> {
    enumerate_candidates_with_mode(
        grid,
        obstacles,
        window,
        area,
        query,
        map_width,
        map_height,
        CandidateEnumerationMode::Search,
    )
}

/// Endpoint suggestions need every legal candidate, even if it overlaps the old request
/// endpoints: those endpoints are ignored while the helper finds replacements.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum CandidateEnumerationMode {
    Search,
    EndpointSuggestion,
}

#[allow(clippy::too_many_arguments)]
fn enumerate_candidates_with_mode(
    grid: &TerrainGrid,
    obstacles: &[PixelRect],
    window: PixelRect,
    area: PixelRect,
    query: &WallSearchQuery,
    map_width: i64,
    map_height: i64,
    mode: CandidateEnumerationMode,
) -> Result<(Vec<Candidate>, Vec<u8>), SearchError> {
    let mut groups: Vec<(BuildingSpec, usize, u8)> = Vec::new();
    for (index, &spec) in query.buildings.iter().enumerate() {
        if let Some((_, _, count)) = groups.iter_mut().find(|(old, _, _)| *old == spec) {
            *count += 1;
        } else {
            groups.push((spec, index, 1));
        }
    }
    let group_counts = groups.iter().map(|(_, _, count)| *count).collect();
    let reach = query
        .placement_site
        .map(|site| wall_placement_reach(grid, window, site))
        .transpose()?;
    let mut candidates = Vec::new();
    for (spec_group, (spec, spec_index, _)) in groups.into_iter().enumerate() {
        // `validate_buildings` gives consistent footprint, collision, extent, and overflow errors.
        let prototype = WallBuilding {
            tile: TilePosition { x: 0, y: 0 },
            footprint: spec.footprint,
            collision: spec.collision,
        };
        validate_buildings(&[prototype])?;
        let min_x = div_ceil(i64::from(area.left), super::PIXELS_PER_TILE);
        let min_y = div_ceil(i64::from(area.top), super::PIXELS_PER_TILE);
        let max_x = (i64::from(area.right)
            - i64::from(spec.footprint.width_tiles) * super::PIXELS_PER_TILE)
            .div_euclid(super::PIXELS_PER_TILE);
        let max_y = (i64::from(area.bottom)
            - i64::from(spec.footprint.height_tiles) * super::PIXELS_PER_TILE)
            .div_euclid(super::PIXELS_PER_TILE);
        if min_x > max_x || min_y > max_y {
            continue;
        }
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                let (Ok(x), Ok(y)) = (u32::try_from(x), u32::try_from(y)) else {
                    continue;
                };
                let building = WallBuilding {
                    tile: TilePosition { x, y },
                    footprint: spec.footprint,
                    collision: spec.collision,
                };
                let geometry = validate_buildings(&[building])?
                    .pop()
                    .expect("one geometry");
                if !rect_contains(area, geometry.footprint)
                    || !rect_contains(window, geometry.footprint)
                {
                    continue;
                }
                let Some(site_distance_rank) = query.placement_site.map_or(Some(0), |site| {
                    footprint_center_distance_rank(geometry.footprint, site)
                }) else {
                    continue;
                };
                let center = PixelPosition {
                    x: u32::try_from(
                        (i64::from(geometry.footprint.left) + i64::from(geometry.footprint.right))
                            / 2,
                    )
                    .expect("candidate footprint is inside the map"),
                    y: u32::try_from(
                        (i64::from(geometry.footprint.top) + i64::from(geometry.footprint.bottom))
                            / 2,
                    )
                    .expect("candidate footprint is inside the map"),
                };
                if reach
                    .as_ref()
                    .is_some_and(|reach| !reach.contains_pixel(center))
                {
                    continue;
                }
                let mut placement = super::WallPlacement {
                    footprint: geometry.footprint,
                    collision: geometry.collision,
                    issues: Vec::new(),
                };
                assess_placements(
                    grid,
                    map_width,
                    map_height,
                    obstacles,
                    &[geometry],
                    std::slice::from_mut(&mut placement),
                );
                if placement.issues.is_empty()
                    && query.placement_elevation.is_none_or(|height| {
                        footprint_has_elevation(grid, geometry.footprint, height)
                    })
                    && (mode == CandidateEnumerationMode::EndpointSuggestion
                        || candidate_keeps_endpoints_open(geometry.collision, query))
                {
                    candidates.push(Candidate {
                        spec_index,
                        spec_group,
                        building,
                        footprint: geometry.footprint,
                        collision: geometry.collision,
                        site_distance_rank,
                    });
                    if candidates.len() > MAX_CANDIDATE_PLACEMENTS {
                        return Err(SearchError::TooManyCandidatePlacements {
                            actual: candidates.len(),
                        });
                    }
                }
            }
        }
    }
    if query.placement_site.is_some() {
        candidates.sort_by_key(|candidate| {
            (
                candidate.site_distance_rank,
                candidate.spec_index,
                candidate.building.tile.y,
                candidate.building.tile.x,
            )
        });
    } else {
        let midpoint_x = i64::from(query.start.x) + i64::from(query.end.x);
        let midpoint_y = i64::from(query.start.y) + i64::from(query.end.y);
        candidates.sort_by_key(|candidate| {
            let x = i64::from(candidate.footprint.left) + i64::from(candidate.footprint.right);
            let y = i64::from(candidate.footprint.top) + i64::from(candidate.footprint.bottom);
            (
                (x - midpoint_x).unsigned_abs() + (y - midpoint_y).unsigned_abs(),
                candidate.spec_index,
                candidate.building.tile.y,
                candidate.building.tile.x,
            )
        });
    }
    Ok((candidates, group_counts))
}

/// A selected candidate that blocks either endpoint can never belong to a returned layout.
/// `EndpointSuggestion` intentionally does not apply this test because it ignores the old request
/// endpoints while it finds the endpoints to use.
fn candidate_keeps_endpoints_open(collision: PixelRect, query: &WallSearchQuery) -> bool {
    let keeps_open = |mover: CollisionBounds| {
        [query.start, query.end].into_iter().all(|position| {
            let x = i64::from(position.x);
            let y = i64::from(position.y);
            x < i64::from(collision.left) - i64::from(mover.right)
                || x >= i64::from(collision.right) + i64::from(mover.left)
                || y < i64::from(collision.top) - i64::from(mover.bottom)
                || y >= i64::from(collision.bottom) + i64::from(mover.top)
        })
    };
    keeps_open(query.block) && query.allow.is_none_or(keeps_open)
}

fn div_ceil(value: i64, divisor: i64) -> i64 {
    value.div_euclid(divisor) + i64::from(value.rem_euclid(divisor) != 0)
}

fn footprint_center_distance_rank(footprint: PixelRect, site: WallPlacementSite) -> Option<u128> {
    let center_x2 = i128::from(footprint.left) + i128::from(footprint.right);
    let center_y2 = i128::from(footprint.top) + i128::from(footprint.bottom);
    let start_x2 = 2 * i128::from(site.entrance[0].x);
    let start_y2 = 2 * i128::from(site.entrance[0].y);
    let dx = i128::from(site.entrance[1].x) - i128::from(site.entrance[0].x);
    let dy = i128::from(site.entrance[1].y) - i128::from(site.entrance[0].y);
    let segment_squared = dx * dx + dy * dy;
    let point_dx2 = center_x2 - start_x2;
    let point_dy2 = center_y2 - start_y2;
    let projection_twice = point_dx2 * dx + point_dy2 * dy;
    let radius = i128::from(site.max_distance_pixels);
    let four_radius_squared = 4 * radius * radius;
    let endpoint_rank = |x2: i128, y2: i128| {
        let numerator = x2 * x2 + y2 * y2;
        (numerator <= four_radius_squared).then_some((numerator / 4) as u128)
    };
    if projection_twice <= 0 {
        return endpoint_rank(point_dx2, point_dy2);
    }
    if projection_twice >= 2 * segment_squared {
        return endpoint_rank(point_dx2 - 2 * dx, point_dy2 - 2 * dy);
    }
    let cross = point_dx2 * dy - point_dy2 * dx;
    let abs_cross = cross.abs();
    let upper_bound = 2 * radius * (dx.abs() + dy.abs());
    if abs_cross > upper_bound {
        return None;
    }
    let numerator = abs_cross * abs_cross;
    let denominator = 4 * segment_squared;
    (numerator <= four_radius_squared * segment_squared)
        .then_some((numerator / denominator) as u128)
}

fn point_in_placement_site(x: i64, y: i64, site: WallPlacementSite) -> bool {
    // Endpoint sampling is already clipped to the i32-bounded search window. A zero-area rect is
    // sufficient here because the distance helper only reads its doubled center coordinates.
    let x = i32::try_from(x).expect("sample is inside the validated window");
    let y = i32::try_from(y).expect("sample is inside the validated window");
    footprint_center_distance_rank(
        PixelRect {
            left: x,
            top: y,
            right: x,
            bottom: y,
        },
        site,
    )
    .is_some()
}

fn footprint_has_elevation(grid: &TerrainGrid, footprint: PixelRect, elevation: u8) -> bool {
    let width = grid.width() as usize;
    for y in (footprint.top / 8) as usize..(footprint.bottom / 8) as usize {
        for x in (footprint.left / 8) as usize..(footprint.right / 8) as usize {
            if grid.cells()[y * width + x].elevation != elevation {
                return false;
            }
        }
    }
    true
}
fn rect_contains(outer: PixelRect, inner: PixelRect) -> bool {
    outer.left <= inner.left
        && outer.top <= inner.top
        && inner.right <= outer.right
        && inner.bottom <= outer.bottom
}

/// A diagonal path edge can be broken by either forbidden corner side cell, even if no route
/// center is stamped. Include both sides so route-guided pruning remains complete.
fn candidate_touches_witness(
    collision: PixelRect,
    mover: CollisionBounds,
    witness: &[usize],
    window: PixelRect,
    width: usize,
) -> bool {
    let covers = |index: usize| {
        let x = i64::from(window.left) + (index % width) as i64;
        let y = i64::from(window.top) + (index / width) as i64;
        x >= i64::from(collision.left) - i64::from(mover.right)
            && x < i64::from(collision.right) + i64::from(mover.left)
            && y >= i64::from(collision.top) - i64::from(mover.bottom)
            && y < i64::from(collision.bottom) + i64::from(mover.top)
    };
    witness.iter().copied().any(covers)
        || witness.windows(2).any(|edge| {
            let (a, b) = (edge[0], edge[1]);
            let (ax, ay) = (a % width, a / width);
            let (bx, by) = (b % width, b / width);
            ax != bx && ay != by && (covers(ay * width + bx) || covers(by * width + ax))
        })
}

fn stamp_collision_into(
    blocked: &mut [bool],
    window: PixelRect,
    width: usize,
    height: usize,
    collision: PixelRect,
    mover: CollisionBounds,
) {
    let left = (i64::from(collision.left) - i64::from(mover.right)).max(i64::from(window.left));
    let top = (i64::from(collision.top) - i64::from(mover.bottom)).max(i64::from(window.top));
    let right = (i64::from(collision.right) + i64::from(mover.left)).min(i64::from(window.right));
    let bottom = (i64::from(collision.bottom) + i64::from(mover.top)).min(i64::from(window.bottom));
    if left >= right || top >= bottom {
        return;
    }
    let left = usize::try_from(left - i64::from(window.left)).expect("clipped x");
    let top = usize::try_from(top - i64::from(window.top)).expect("clipped y");
    let right = usize::try_from(right - i64::from(window.left)).expect("clipped x");
    let bottom = usize::try_from(bottom - i64::from(window.top)).expect("clipped y");
    for y in top..bottom.min(height) {
        for x in left..right.min(width) {
            blocked[y * width + x] = true;
        }
    }
}

fn route_positions(indices: Vec<usize>, window: PixelRect, width: usize) -> Vec<PixelPosition> {
    indices
        .into_iter()
        .map(|index| PixelPosition {
            x: u32::try_from(i64::from(window.left) + (index % width) as i64)
                .expect("validated coordinate"),
            y: u32::try_from(i64::from(window.top) + (index / width) as i64)
                .expect("validated coordinate"),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::super::{WallPassage, WallQuery, check_wall};
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
                        terrain_buildable: !blocked(x, y),
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
    fn point() -> CollisionBounds {
        CollisionBounds {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
        }
    }
    fn wall() -> CollisionBounds {
        CollisionBounds {
            left: 16,
            top: 16,
            right: 15,
            bottom: 15,
        }
    }
    fn query() -> WallSearchQuery {
        WallSearchQuery {
            window: rect(0, 0, 128, 96),
            placement_area: rect(0, 32, 128, 64),
            placement_elevation: None,
            placement_site: None,
            start: PixelPosition { x: 4, y: 48 },
            end: PixelPosition { x: 123, y: 48 },
            block: CollisionBounds {
                left: 4,
                top: 4,
                right: 4,
                bottom: 4,
            },
            allow: None,
            buildings: vec![BuildingSpec {
                footprint: DepotFootprint {
                    width_tiles: 1,
                    height_tiles: 1,
                },
                collision: wall(),
            }],
            max_states: 1_000,
            max_results: 16,
        }
    }
    fn corridor() -> TerrainGrid {
        terrain(16, 12, |_, y| !(4..9).contains(&y))
    }
    fn run(mut search: WallSearch, slice: u32) -> WallSearchResult {
        while !search.is_done() {
            search.step(slice);
        }
        search.result().clone()
    }

    type LayoutKey = Vec<(usize, u32, u32, u32, u32, u32, u32, u32, u32)>;

    fn placement_key(
        spec_index: usize,
        building: WallBuilding,
    ) -> (usize, u32, u32, u32, u32, u32, u32, u32, u32) {
        (
            spec_index,
            building.tile.x,
            building.tile.y,
            building.footprint.width_tiles,
            building.footprint.height_tiles,
            building.collision.left,
            building.collision.top,
            building.collision.right,
            building.collision.bottom,
        )
    }

    fn oracle_layout_key(placements: &[(usize, WallBuilding)]) -> LayoutKey {
        let mut key: LayoutKey = placements
            .iter()
            .map(|&(spec_index, building)| placement_key(spec_index, building))
            .collect();
        key.sort_unstable();
        key
    }

    fn search_layout_key(layout: &WallLayout) -> LayoutKey {
        let mut key: LayoutKey = layout
            .buildings
            .iter()
            .map(|placement| placement_key(placement.spec_index, placement.building))
            .collect();
        key.sort_unstable();
        key
    }

    /// Independently enumerates tile-aligned proposals, then asks the public checker whether a
    /// proposal is legal and whether each complete layout blocks or preserves passage. It shares
    /// no candidate filtering or witness logic with the search itself.
    fn oracle_minimal_layouts(
        grid: &TerrainGrid,
        request: &WallSearchQuery,
    ) -> (usize, Vec<LayoutKey>) {
        assert!(request.placement_site.is_none());
        assert!(request.placement_elevation.is_none());
        let mut groups: Vec<(usize, BuildingSpec, usize)> = Vec::new();
        for (spec_index, &spec) in request.buildings.iter().enumerate() {
            if let Some((_, _, count)) = groups.iter_mut().find(|(_, old, _)| *old == spec) {
                *count += 1;
            } else {
                groups.push((spec_index, spec, 1));
            }
        }

        let mut candidates = Vec::new();
        for (group, (spec_index, spec, _)) in groups.iter().copied().enumerate() {
            for tile_y in 0..grid.height() / 4 {
                for tile_x in 0..grid.width() / 4 {
                    let left = i32::try_from(tile_x * 32).unwrap();
                    let top = i32::try_from(tile_y * 32).unwrap();
                    let right = left + i32::try_from(spec.footprint.width_tiles * 32).unwrap();
                    let bottom = top + i32::try_from(spec.footprint.height_tiles * 32).unwrap();
                    if left < request.placement_area.left
                        || top < request.placement_area.top
                        || right > request.placement_area.right
                        || bottom > request.placement_area.bottom
                        || left < request.window.left
                        || top < request.window.top
                        || right > request.window.right
                        || bottom > request.window.bottom
                    {
                        continue;
                    }
                    let building = WallBuilding {
                        tile: TilePosition {
                            x: tile_x,
                            y: tile_y,
                        },
                        footprint: spec.footprint,
                        collision: spec.collision,
                    };
                    let block = check_wall(
                        grid,
                        &[],
                        &WallQuery {
                            window: request.window,
                            start: request.start,
                            end: request.end,
                            mover: request.block,
                            buildings: vec![building],
                        },
                    )
                    .unwrap();
                    if !block.placements[0].issues.is_empty()
                        || matches!(
                            block.passage,
                            WallPassage::StartBlocked | WallPassage::EndBlocked
                        )
                    {
                        continue;
                    }
                    if let Some(allow) = request.allow {
                        let allow = check_wall(
                            grid,
                            &[],
                            &WallQuery {
                                window: request.window,
                                start: request.start,
                                end: request.end,
                                mover: allow,
                                buildings: vec![building],
                            },
                        )
                        .unwrap();
                        if matches!(
                            allow.passage,
                            WallPassage::StartBlocked | WallPassage::EndBlocked
                        ) {
                            continue;
                        }
                    }
                    candidates.push((group, spec_index, building));
                }
            }
        }

        let mut layouts = Vec::new();
        for mask in 1_usize..(1_usize << candidates.len()) {
            let mut used = vec![0_usize; groups.len()];
            let mut selected = Vec::new();
            for (index, &(group, spec_index, building)) in candidates.iter().enumerate() {
                if mask & (1 << index) != 0 {
                    used[group] += 1;
                    selected.push((spec_index, building));
                }
            }
            if used
                .iter()
                .zip(groups.iter())
                .any(|(used, (_, _, available))| *used > *available)
            {
                continue;
            }
            let buildings: Vec<_> = selected.iter().map(|(_, building)| *building).collect();
            let block = check_wall(
                grid,
                &[],
                &WallQuery {
                    window: request.window,
                    start: request.start,
                    end: request.end,
                    mover: request.block,
                    buildings: buildings.clone(),
                },
            )
            .unwrap();
            if !block
                .placements
                .iter()
                .all(|placement| placement.issues.is_empty())
                || block.passage != WallPassage::BlockedWithinWindow
            {
                continue;
            }
            if let Some(allow) = request.allow {
                let allow = check_wall(
                    grid,
                    &[],
                    &WallQuery {
                        window: request.window,
                        start: request.start,
                        end: request.end,
                        mover: allow,
                        buildings: buildings.clone(),
                    },
                )
                .unwrap();
                if allow.passage != WallPassage::Open {
                    continue;
                }
            }
            let minimal = (0..buildings.len()).all(|removed| {
                check_wall(
                    grid,
                    &[],
                    &WallQuery {
                        window: request.window,
                        start: request.start,
                        end: request.end,
                        mover: request.block,
                        buildings: buildings
                            .iter()
                            .enumerate()
                            .filter_map(|(index, building)| (index != removed).then_some(*building))
                            .collect(),
                    },
                )
                .unwrap()
                .passage
                    == WallPassage::Open
            });
            if minimal {
                layouts.push(oracle_layout_key(&selected));
            }
        }
        layouts.sort_unstable();
        layouts.dedup();
        (candidates.len(), layouts)
    }

    fn reach_terrain(
        width: u32,
        height: u32,
        cell: impl Fn(u32, u32) -> TerrainCell + Copy,
    ) -> TerrainGrid {
        TerrainGrid::from_cells(
            width,
            height,
            (0..height)
                .flat_map(|y| (0..width).map(move |x| cell(x, y)))
                .collect(),
        )
        .unwrap()
    }

    fn walkable_cell(elevation: u8) -> TerrainCell {
        TerrainCell {
            walkable: true,
            terrain_buildable: true,
            elevation,
            ramp: false,
        }
    }

    #[test]
    fn placement_reach_excludes_disconnected_same_height_ledge() {
        let grid = reach_terrain(12, 8, |x, _| TerrainCell {
            walkable: x != 6,
            terrain_buildable: x != 6,
            elevation: 1,
            ramp: false,
        });
        let reach = wall_placement_reach(
            &grid,
            rect(0, 0, 96, 64),
            WallPlacementSite {
                entrance: [
                    PixelPosition { x: 20, y: 16 },
                    PixelPosition { x: 20, y: 48 },
                ],
                max_distance_pixels: 64,
            },
        )
        .unwrap();
        assert!(reach.contains_pixel(PixelPosition { x: 20, y: 32 }));
        assert!(!reach.contains_pixel(PixelPosition { x: 60, y: 32 }));
    }

    #[test]
    fn placement_reach_crosses_a_nearby_ramp_without_an_elevation_choice() {
        let grid = reach_terrain(12, 8, |x, _| TerrainCell {
            elevation: u8::from(x >= 6),
            ramp: x == 5,
            ..walkable_cell(0)
        });
        let reach = wall_placement_reach(
            &grid,
            rect(0, 0, 96, 64),
            WallPlacementSite {
                entrance: [
                    PixelPosition { x: 20, y: 16 },
                    PixelPosition { x: 20, y: 48 },
                ],
                max_distance_pixels: 64,
            },
        )
        .unwrap();
        assert!(reach.contains_pixel(PixelPosition { x: 60, y: 32 }));
    }

    #[test]
    fn placement_reach_keeps_finite_capsule_and_exact_window_bounds() {
        let grid = terrain(12, 12, |_, _| false);
        let reach = wall_placement_reach(
            &grid,
            rect(9, 8, 87, 88),
            WallPlacementSite {
                entrance: [
                    PixelPosition { x: 20, y: 24 },
                    PixelPosition { x: 20, y: 72 },
                ],
                max_distance_pixels: 0,
            },
        )
        .unwrap();
        // This cell is collinear with the entrance but begins beyond its finite endpoint.
        assert!(!reach.contains_pixel(PixelPosition { x: 20, y: 84 }));
        // Reach the partial edge cell, then distinguish pixels inside and outside the exact
        // window within that same cell.
        let partial = wall_placement_reach(
            &grid,
            rect(9, 8, 87, 88),
            WallPlacementSite {
                entrance: [
                    PixelPosition { x: 12, y: 24 },
                    PixelPosition { x: 12, y: 72 },
                ],
                max_distance_pixels: 8,
            },
        )
        .unwrap();
        assert!(partial.contains_pixel(PixelPosition { x: 9, y: 32 }));
        assert!(!partial.contains_pixel(PixelPosition { x: 8, y: 32 }));
    }

    #[test]
    fn site_candidates_reject_a_ledge_reached_only_outside_the_local_capsule() {
        // The left and right landings are globally connected around either end of the wall of
        // unwalkable terrain. Both paths leave the finite site's radius capsule, so a local wall
        // search must retain the near landing and reject the otherwise in-radius right ledge.
        let grid = terrain(20, 40, |x, y| x / 4 == 2 && (8..=33).contains(&y));
        assert!(
            grid.route(WalkPosition { x: 2, y: 22 }, WalkPosition { x: 14, y: 22 },)
                .unwrap()
                .is_some()
        );
        let mut request = query();
        request.window = rect(0, 0, 160, 320);
        request.placement_area = request.window;
        request.placement_site = Some(WallPlacementSite {
            entrance: [
                PixelPosition { x: 16, y: 160 },
                PixelPosition { x: 16, y: 192 },
            ],
            max_distance_pixels: 96,
        });
        let (candidates, _) = enumerate_candidates(
            &grid,
            &[],
            request.window,
            request.placement_area,
            &request,
            160,
            320,
        )
        .unwrap();
        assert!(
            candidates
                .iter()
                .any(|candidate| candidate.building.tile == TilePosition { x: 1, y: 5 })
        );
        assert!(
            !candidates
                .iter()
                .any(|candidate| candidate.building.tile == TilePosition { x: 3, y: 5 })
        );
    }
    #[test]
    fn placement_reach_does_not_cut_a_diagonal_corner() {
        let grid = reach_terrain(3, 3, |x, y| TerrainCell {
            walkable: (x, y) == (0, 0) || (x, y) == (1, 1),
            terrain_buildable: true,
            elevation: 0,
            ramp: false,
        });
        let reach = wall_placement_reach(
            &grid,
            rect(0, 0, 24, 24),
            WallPlacementSite {
                entrance: [PixelPosition { x: 1, y: 1 }, PixelPosition { x: 7, y: 7 }],
                max_distance_pixels: 32,
            },
        )
        .unwrap();
        assert!(reach.contains_pixel(PixelPosition { x: 4, y: 4 }));
        assert!(!reach.contains_pixel(PixelPosition { x: 12, y: 12 }));
    }

    #[test]
    fn placement_reach_zero_radius_seeds_open_edge_but_not_endpoint_corner() {
        let edge_grid = terrain(3, 4, |_, _| false);
        let edge_reach = wall_placement_reach(
            &edge_grid,
            rect(0, 0, 24, 32),
            WallPlacementSite {
                entrance: [PixelPosition { x: 8, y: 8 }, PixelPosition { x: 8, y: 24 }],
                max_distance_pixels: 0,
            },
        )
        .unwrap();
        assert!(edge_reach.contains_pixel(PixelPosition { x: 4, y: 12 }));
        assert!(edge_reach.contains_pixel(PixelPosition { x: 12, y: 12 }));

        let corner_grid = reach_terrain(2, 2, |x, y| TerrainCell {
            walkable: (x, y) == (0, 0),
            terrain_buildable: true,
            elevation: 0,
            ramp: false,
        });
        let corner_reach = wall_placement_reach(
            &corner_grid,
            rect(0, 0, 16, 16),
            WallPlacementSite {
                entrance: [PixelPosition { x: 8, y: 8 }, PixelPosition { x: 8, y: 16 }],
                max_distance_pixels: 0,
            },
        )
        .unwrap();
        assert!(!corner_reach.contains_pixel(PixelPosition { x: 4, y: 4 }));
    }

    #[test]
    fn site_candidates_can_have_legal_footprints_beyond_the_radius() {
        let grid = terrain(16, 12, |_, _| false);
        let mut request = query();
        request.placement_area = rect(0, 0, 128, 96);
        request.placement_site = Some(WallPlacementSite {
            entrance: [
                PixelPosition { x: 32, y: 16 },
                PixelPosition { x: 32, y: 48 },
            ],
            max_distance_pixels: 0,
        });
        request.buildings = vec![BuildingSpec {
            footprint: DepotFootprint {
                width_tiles: 2,
                height_tiles: 1,
            },
            collision: wall(),
        }];
        let (candidates, _) = enumerate_candidates(
            &grid,
            &[],
            request.window,
            request.placement_area,
            &request,
            128,
            96,
        )
        .unwrap();
        assert!(candidates.iter().any(|candidate| {
            candidate.building.tile == TilePosition { x: 0, y: 0 }
                && candidate.footprint.right - candidate.footprint.left > 0
        }));
    }
    #[test]
    fn endpoint_filter_is_search_only_for_each_mover() {
        let grid = terrain(16, 12, |_, _| false);
        for (name, block, allow) in [
            (
                "block",
                CollisionBounds {
                    left: 4,
                    top: 0,
                    right: 16,
                    bottom: 0,
                },
                Some(point()),
            ),
            (
                "allow",
                point(),
                Some(CollisionBounds {
                    left: 4,
                    top: 0,
                    right: 16,
                    bottom: 0,
                }),
            ),
        ] {
            let request = WallSearchQuery {
                window: rect(0, 0, 128, 96),
                placement_area: rect(32, 32, 64, 64),
                placement_elevation: None,
                placement_site: None,
                start: PixelPosition { x: 20, y: 48 },
                end: PixelPosition { x: 100, y: 48 },
                block,
                allow,
                buildings: vec![BuildingSpec {
                    footprint: DepotFootprint {
                        width_tiles: 1,
                        height_tiles: 1,
                    },
                    collision: wall(),
                }],
                max_states: 32,
                max_results: 8,
            };
            let (suggestion_candidates, _) = enumerate_candidates(
                &grid,
                &[],
                request.window,
                request.placement_area,
                &request,
                128,
                96,
            )
            .unwrap();
            assert_eq!(suggestion_candidates.len(), 1, "{name}");
            let (search_candidates, _) = enumerate_search_candidates(
                &grid,
                &[],
                request.window,
                request.placement_area,
                &request,
                128,
                96,
            )
            .unwrap();
            assert!(search_candidates.is_empty(), "{name}");
            assert_eq!(
                WallSearch::new(&grid, &[], request)
                    .unwrap()
                    .result()
                    .candidate_placements,
                0,
                "{name}"
            );
        }
    }

    #[test]
    fn witness_pruning_matches_public_oracle_on_seeded_corridors() {
        let mut state = 0x6D2B_79F5_u32;
        let mut zero_oracle_cases = 0;
        let mut nonzero_oracle_cases = 0;
        for case_index in 0..64 {
            state = state.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            let reference_case = case_index % 8 == 0;
            let map_walk_width = if state & 1 == 0 { 16 } else { 20 };
            let map_pixels_width = map_walk_width * 8;
            let top_tile = 1 + (state >> 1) % 2;
            let opening_top = top_tile * 32;
            let columns = if reference_case {
                1
            } else {
                1 + (state >> 3) % 2
            };
            let map_tiles_width = map_walk_width / 4;
            let column_start = map_tiles_width / 2 - u32::from(columns == 2);
            let force_zero = case_index % 8 == 1;
            let notch = (!reference_case && case_index % 3 == 0).then_some((
                map_walk_width / 2,
                top_tile * 4 + if state & 0x10 == 0 { 0 } else { 10 },
            ));
            let buildable_hole = (!reference_case && !force_zero && case_index % 5 == 0)
                .then_some((column_start * 4 + 1, top_tile * 4 + 5));
            let grid = TerrainGrid::from_cells(
                map_walk_width,
                24,
                (0..24)
                    .flat_map(|y| {
                        (0..map_walk_width).map(move |x| {
                            let walkable = (top_tile * 4..top_tile * 4 + 12).contains(&y)
                                && notch != Some((x, y));
                            let in_construction = x >= column_start * 4
                                && x < (column_start + columns) * 4
                                && (top_tile * 4..top_tile * 4 + 12).contains(&y);
                            TerrainCell {
                                walkable,
                                terrain_buildable: walkable
                                    && !(force_zero && in_construction)
                                    && buildable_hole != Some((x, y)),
                                elevation: 0,
                                ramp: false,
                            }
                        })
                    })
                    .collect(),
            )
            .unwrap();
            let small_collision = if reference_case {
                wall()
            } else {
                CollisionBounds {
                    left: 14 + state % 4,
                    top: 14 + (state >> 4) % 4,
                    right: 13 + (state >> 8) % 4,
                    bottom: 13 + (state >> 12) % 4,
                }
            };
            let request = WallSearchQuery {
                window: rect(
                    0,
                    opening_top as i32,
                    map_pixels_width as i32,
                    (opening_top + 96) as i32,
                ),
                placement_area: rect(
                    (column_start * 32) as i32,
                    opening_top as i32,
                    ((column_start + columns) * 32) as i32,
                    (opening_top + 96) as i32,
                ),
                placement_elevation: None,
                placement_site: None,
                start: PixelPosition {
                    x: 12,
                    y: opening_top + 48,
                },
                end: PixelPosition {
                    x: map_pixels_width - 13,
                    y: opening_top + 48,
                },
                block: if reference_case {
                    CollisionBounds {
                        left: 8,
                        top: 16,
                        right: 8,
                        bottom: 16,
                    }
                } else {
                    CollisionBounds {
                        left: 6 + (state >> 16) % 4,
                        top: 16 + (state >> 20) % 4,
                        right: 6 + (state >> 24) % 4,
                        bottom: 16 + (state >> 28) % 4,
                    }
                },
                allow: (state & 0x20 != 0).then_some(point()),
                buildings: vec![
                    BuildingSpec {
                        footprint: DepotFootprint {
                            width_tiles: 1,
                            height_tiles: 1,
                        },
                        collision: small_collision,
                    },
                    BuildingSpec {
                        footprint: DepotFootprint {
                            width_tiles: 1,
                            height_tiles: 1,
                        },
                        collision: small_collision,
                    },
                    BuildingSpec {
                        footprint: DepotFootprint {
                            width_tiles: 1,
                            height_tiles: 2,
                        },
                        collision: small_collision,
                    },
                ],
                max_states: 100_000,
                max_results: 32,
            };
            let (candidate_count, mut expected) = oracle_minimal_layouts(&grid, &request);
            assert!(candidate_count <= 10, "seed {state:#010x}");
            if expected.is_empty() {
                zero_oracle_cases += 1;
            } else {
                nonzero_oracle_cases += 1;
            }
            assert!(expected.len() < request.max_results, "seed {state:#010x}");

            let result = run(WallSearch::new(&grid, &[], request.clone()).unwrap(), 1);
            assert_eq!(
                result.reason,
                WallSearchReason::Exhausted,
                "seed {state:#010x}"
            );
            assert!(
                result.explored_states < request.max_states,
                "seed {state:#010x}"
            );
            assert!(
                result.layouts.len() < request.max_results,
                "seed {state:#010x}"
            );
            assert_eq!(
                result.candidate_placements, candidate_count,
                "seed {state:#010x}"
            );

            let mut found: Vec<_> = result.layouts.iter().map(search_layout_key).collect();
            found.sort_unstable();
            expected.sort_unstable();
            assert_eq!(found, expected, "seed {state:#010x}");
        }
        assert!(zero_oracle_cases > 0);
        assert!(nonzero_oracle_cases > 0);
    }

    #[test]
    fn finds_a_known_corridor_wall_and_public_checker_verifies_it() {
        let grid = corridor();
        let result = run(WallSearch::new(&grid, &[], query()).unwrap(), 1);
        assert_eq!(result.reason, WallSearchReason::Exhausted);
        let layout = result.layouts.first().expect("corridor wall");
        let check = check_wall(
            &grid,
            &[],
            &WallQuery {
                window: rect(0, 0, 128, 96),
                start: PixelPosition { x: 4, y: 48 },
                end: PixelPosition { x: 123, y: 48 },
                mover: CollisionBounds {
                    left: 4,
                    top: 4,
                    right: 4,
                    bottom: 4,
                },
                buildings: layout
                    .buildings
                    .iter()
                    .map(|placement| placement.building)
                    .collect(),
            },
        )
        .unwrap();
        assert_eq!(check.passage, WallPassage::BlockedWithinWindow);
    }

    #[test]
    fn preserves_a_smaller_optional_mover() {
        let grid = corridor();
        let mut request = query();
        request.allow = Some(point());
        let result = run(WallSearch::new(&grid, &[], request).unwrap(), 4);
        assert!(
            result
                .layouts
                .iter()
                .any(|layout| !layout.allow_route.is_empty())
        );
    }

    #[test]
    fn validates_baselines_endpoints_and_limits() {
        let blocked = terrain(16, 12, |x, _| x == 7);
        assert!(matches!(
            WallSearch::new(&blocked, &[], query()),
            Err(SearchError::BaselineDisconnected { .. })
        ));
        let grid = corridor();
        let mut outside = query();
        outside.start.x = 128;
        assert!(matches!(
            WallSearch::new(&grid, &[], outside),
            Err(SearchError::Wall(WallError::EndpointOutsideWindow { .. }))
        ));
        let mut invalid_area = query();
        invalid_area.placement_area.left = -1;
        assert!(matches!(
            WallSearch::new(&grid, &[], invalid_area),
            Err(SearchError::InvalidPlacementArea)
        ));
        let mut zero = query();
        zero.max_states = 0;
        assert!(matches!(
            WallSearch::new(&grid, &[], zero),
            Err(SearchError::InvalidStateLimit { .. })
        ));
        let mut no_results = query();
        no_results.max_results = 0;
        assert!(matches!(
            WallSearch::new(&grid, &[], no_results),
            Err(SearchError::InvalidResultLimit { .. })
        ));
    }

    #[test]
    fn unbuildable_candidate_terrain_is_never_selected() {
        let grid = terrain(16, 12, |x, y| !(4..9).contains(&y) || (x == 4 && y == 4));
        let mut request = query();
        request.placement_area = rect(32, 32, 64, 64);
        let result = run(WallSearch::new(&grid, &[], request).unwrap(), 1);
        assert_eq!(result.reason, WallSearchReason::Exhausted);
        assert_eq!(result.candidate_placements, 0);
        assert!(result.layouts.is_empty());
    }

    #[test]
    fn diagonal_witness_includes_no_corner_cutting_side_cells() {
        // The route 0 -> 3 is diagonal in a 2x2 window. A stamp on center 1 breaks that
        // diagonal by blocking its horizontal side even though it does not cover either endpoint.
        assert!(candidate_touches_witness(
            rect(1, 0, 2, 1),
            point(),
            &[0, 3],
            rect(0, 0, 2, 2),
            2,
        ));
    }

    #[test]
    fn duplicate_inventory_and_steps_are_deterministic() {
        let grid = corridor();
        let mut request = query();
        request.buildings.push(request.buildings[0]);
        let one = run(WallSearch::new(&grid, &[], request.clone()).unwrap(), 1);
        let many = run(WallSearch::new(&grid, &[], request).unwrap(), 100);
        assert_eq!(one, many);
        assert!(one.layouts.iter().all(|layout| {
            layout
                .buildings
                .iter()
                .all(|placement| placement.spec_index == 0)
        }));
    }

    #[test]
    fn lazy_dfs_reaches_a_three_building_minimal_wall_under_a_small_budget() {
        // A 96px-tall opening requires three stacked 32px collision boxes at one x position.
        // The first witness takes the lower detour, then the upper detour, so this also checks
        // that pending siblings do not consume the four evaluated-state budget.
        let grid = terrain(16, 20, |_, y| !(4..16).contains(&y));
        let mut request = WallSearchQuery {
            window: rect(0, 0, 128, 160),
            placement_area: rect(32, 32, 64, 128),
            placement_elevation: None,
            placement_site: None,
            start: PixelPosition { x: 4, y: 80 },
            end: PixelPosition { x: 123, y: 80 },
            block: point(),
            allow: None,
            buildings: vec![
                BuildingSpec {
                    footprint: DepotFootprint {
                        width_tiles: 1,
                        height_tiles: 1
                    },
                    collision: wall(),
                };
                3
            ],
            max_states: 4,
            max_results: 1,
        };
        let result = run(WallSearch::new(&grid, &[], request.clone()).unwrap(), 1);
        assert_eq!(result.reason, WallSearchReason::ResultLimit);
        let layout = result.layouts.first().expect("three-building wall");
        assert_eq!(layout.buildings.len(), 3);
        assert_eq!(result.explored_states, 4);
        let check = check_wall(
            &grid,
            &[],
            &WallQuery {
                window: request.window,
                start: request.start,
                end: request.end,
                mover: request.block,
                buildings: layout
                    .buildings
                    .iter()
                    .map(|placement| placement.building)
                    .collect(),
            },
        )
        .unwrap();
        assert_eq!(check.passage, WallPassage::BlockedWithinWindow);
        // Independent exhaustive oracle over this deliberately tiny candidate domain. It verifies
        // both that the route-guided DFS did not prune the only wall and that it retained no
        // redundant layout in preference to the minimal one.
        let choices: Vec<_> = (1..4)
            .map(|y| WallBuilding {
                tile: TilePosition { x: 1, y },
                footprint: DepotFootprint {
                    width_tiles: 1,
                    height_tiles: 1,
                },
                collision: wall(),
            })
            .collect();
        let mut oracle = Vec::new();
        for mask in 1_u8..(1 << choices.len()) {
            let buildings: Vec<_> = choices
                .iter()
                .enumerate()
                .filter_map(|(index, building)| (mask & (1 << index) != 0).then_some(*building))
                .collect();
            let sealed = check_wall(
                &grid,
                &[],
                &WallQuery {
                    window: request.window,
                    start: request.start,
                    end: request.end,
                    mover: request.block,
                    buildings: buildings.clone(),
                },
            )
            .unwrap()
            .passage
                == WallPassage::BlockedWithinWindow;
            let minimal = sealed
                && (0..buildings.len()).all(|removed| {
                    check_wall(
                        &grid,
                        &[],
                        &WallQuery {
                            window: request.window,
                            start: request.start,
                            end: request.end,
                            mover: request.block,
                            buildings: buildings
                                .iter()
                                .enumerate()
                                .filter_map(|(index, building)| {
                                    (index != removed).then_some(*building)
                                })
                                .collect(),
                        },
                    )
                    .unwrap()
                    .passage
                        == WallPassage::Open
                });
            if minimal {
                oracle.push(buildings);
            }
        }
        assert_eq!(oracle.len(), 1);
        let mut found: Vec<_> = layout
            .buildings
            .iter()
            .map(|placement| placement.building)
            .collect();
        let mut expected = oracle.pop().unwrap();
        found.sort_by_key(|building| (building.tile.y, building.tile.x));
        expected.sort_by_key(|building| (building.tile.y, building.tile.x));
        assert_eq!(found, expected);

        for removed in 0..layout.buildings.len() {
            let check = check_wall(
                &grid,
                &[],
                &WallQuery {
                    window: request.window,
                    start: request.start,
                    end: request.end,
                    mover: request.block,
                    buildings: layout
                        .buildings
                        .iter()
                        .enumerate()
                        .filter_map(|(index, placement)| {
                            (index != removed).then_some(placement.building)
                        })
                        .collect(),
                },
            )
            .unwrap();
            assert_eq!(check.passage, WallPassage::Open);
        }
        request.max_states = 3;
        assert_eq!(
            run(WallSearch::new(&grid, &[], request).unwrap(), 1).reason,
            WallSearchReason::StateLimit
        );
    }

    #[test]
    fn obstacles_and_footprint_overlap_are_rejected_from_layouts() {
        let grid = corridor();
        let mut blocked_by_obstacle = query();
        blocked_by_obstacle.placement_area = rect(32, 32, 64, 64);
        let result = run(
            WallSearch::new(&grid, &[rect(40, 40, 41, 41)], blocked_by_obstacle).unwrap(),
            1,
        );
        assert_eq!(result.candidate_placements, 0);

        let mut inventory = query();
        inventory.buildings.push(inventory.buildings[0]);
        let result = run(WallSearch::new(&grid, &[], inventory).unwrap(), 1);
        for layout in result.layouts {
            for (index, placement) in layout.buildings.iter().enumerate() {
                let geometry = validate_buildings(&[placement.building])
                    .unwrap()
                    .pop()
                    .unwrap();
                for other in layout.buildings.iter().skip(index + 1) {
                    let other = validate_buildings(&[other.building])
                        .unwrap()
                        .pop()
                        .unwrap();
                    assert!(!intersects(geometry.footprint, other.footprint));
                }
            }
        }
    }

    #[test]
    fn elevation_filter_and_allow_validation_are_enforced() {
        let mut cells = vec![
            TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            16 * 12
        ];
        cells[4 * 16 + 5].elevation = 1;
        let grid = TerrainGrid::from_cells(16, 12, cells).unwrap();
        let mut request = query();
        request.placement_area = rect(32, 32, 64, 64);
        request.placement_elevation = Some(0);
        assert_eq!(
            WallSearch::new(&grid, &[], request.clone())
                .unwrap()
                .result()
                .candidate_placements,
            0
        );
        request.placement_elevation = None;
        assert_eq!(
            WallSearch::new(&grid, &[], request.clone())
                .unwrap()
                .result()
                .candidate_placements,
            1
        );
        request.placement_elevation = Some(3);
        assert!(matches!(
            WallSearch::new(&grid, &[], request.clone()),
            Err(SearchError::InvalidPlacementElevation { actual: 3 })
        ));
        request.placement_elevation = None;
        request.allow = Some(CollisionBounds {
            left: 8_193,
            top: 0,
            right: 0,
            bottom: 0,
        });
        assert!(matches!(
            suggest_wall_endpoints(
                &grid,
                &[],
                &request,
                [
                    PixelPosition { x: 48, y: 32 },
                    PixelPosition { x: 48, y: 64 }
                ]
            ),
            Err(SearchError::Wall(WallError::MoverExtentTooLarge))
        ));
    }

    #[test]
    fn suggestion_excludes_all_candidate_collision_bodies_and_reports_no_room() {
        let grid = terrain(16, 12, |_, _| false);
        let mut request = query();
        request.placement_area = rect(32, 32, 96, 64);
        request.block = CollisionBounds {
            left: 7,
            top: 3,
            right: 11,
            bottom: 5,
        };
        request.allow = Some(CollisionBounds {
            left: 2,
            top: 4,
            right: 13,
            bottom: 1,
        });
        request.buildings = vec![BuildingSpec {
            footprint: DepotFootprint {
                width_tiles: 1,
                height_tiles: 1,
            },
            collision: CollisionBounds {
                left: 23,
                top: 19,
                right: 14,
                bottom: 12,
            },
        }];
        let endpoints = suggest_wall_endpoints(
            &grid,
            &[],
            &request,
            [
                PixelPosition { x: 64, y: 36 },
                PixelPosition { x: 64, y: 60 },
            ],
        )
        .unwrap();
        let (candidates, _) = enumerate_candidates(
            &grid,
            &[],
            request.window,
            request.placement_area,
            &request,
            128,
            96,
        )
        .unwrap();
        for endpoint in endpoints {
            for candidate in &candidates {
                for mover in [request.block, request.allow.unwrap()] {
                    assert!(
                        i64::from(endpoint.x)
                            < i64::from(candidate.collision.left) - i64::from(mover.right)
                            || i64::from(endpoint.x)
                                >= i64::from(candidate.collision.right) + i64::from(mover.left)
                            || i64::from(endpoint.y)
                                < i64::from(candidate.collision.top) - i64::from(mover.bottom)
                            || i64::from(endpoint.y)
                                >= i64::from(candidate.collision.bottom) + i64::from(mover.top)
                    );
                }
            }
        }
        request.placement_area = request.window;
        assert!(matches!(
            suggest_wall_endpoints(
                &grid,
                &[],
                &request,
                [
                    PixelPosition { x: 48, y: 32 },
                    PixelPosition { x: 48, y: 64 }
                ],
            ),
            Err(SearchError::NoSafeEndpointPair)
        ));
    }
    #[test]
    fn suggestion_scans_lateral_room_and_requires_a_shared_mover_component() {
        let grid = terrain(16, 12, |_, _| false);
        let mut request = query();
        request.placement_area = rect(32, 0, 96, 64);
        let endpoints = suggest_wall_endpoints(
            &grid,
            &[],
            &request,
            [
                PixelPosition { x: 64, y: 16 },
                PixelPosition { x: 95, y: 17 },
            ],
        )
        .unwrap();
        // The negative normal ray leaves the map above the construction area. The chosen A is
        // still safe because the bounded scan finds lateral room to the east.
        assert!(endpoints[0].x >= 96 && endpoints[0].y < 64);

        request.placement_area = rect(48, 32, 80, 64);
        request.block = point();
        request.allow = Some(CollisionBounds {
            left: 0,
            top: 4,
            right: 0,
            bottom: 4,
        });
        let split = [rect(60, 0, 68, 44), rect(60, 52, 68, 96)];
        let point_only = WallSearchQuery {
            allow: None,
            ..request.clone()
        };
        assert!(
            suggest_wall_endpoints(
                &grid,
                &split,
                &point_only,
                [
                    PixelPosition { x: 64, y: 36 },
                    PixelPosition { x: 64, y: 60 }
                ],
            )
            .is_ok()
        );
        assert!(matches!(
            suggest_wall_endpoints(
                &grid,
                &split,
                &request,
                [
                    PixelPosition { x: 64, y: 36 },
                    PixelPosition { x: 64, y: 60 }
                ],
            ),
            Err(SearchError::NoSafeEndpointPair)
        ));
    }
    #[test]
    fn suggestion_ignores_old_endpoints_and_accepts_boundary_geometry() {
        let grid = terrain(16, 12, |_, _| false);
        let mut request = query();
        request.placement_area = rect(32, 32, 96, 64);
        request.start = PixelPosition {
            x: u32::MAX,
            y: u32::MAX,
        };
        request.end = request.start;
        let entrance = [
            PixelPosition { x: 64, y: 0 },
            PixelPosition { x: 64, y: 96 },
        ];
        let endpoints = suggest_wall_endpoints(&grid, &[], &request, entrance).unwrap();
        assert_eq!(
            endpoints,
            suggest_wall_endpoints(&grid, &[], &request, entrance).unwrap()
        );
        assert!(matches!(
            suggest_wall_endpoints(&grid, &[], &request, [entrance[0]; 2]),
            Err(SearchError::DegenerateEntrance)
        ));
        assert!(matches!(
            suggest_wall_endpoints(
                &grid,
                &[],
                &request,
                [entrance[0], PixelPosition { x: 64, y: 97 }]
            ),
            Err(SearchError::EntranceOutsideMap { .. })
        ));
    }

    #[test]
    fn state_and_result_limits_are_reported() {
        let grid = corridor();
        let mut state = query();
        state.max_states = 1;
        assert_eq!(
            run(WallSearch::new(&grid, &[], state).unwrap(), 8).reason,
            WallSearchReason::StateLimit
        );
        let mut results = query();
        results.max_results = 1;
        assert_eq!(
            run(WallSearch::new(&grid, &[], results).unwrap(), 8).reason,
            WallSearchReason::ResultLimit
        );
    }

    #[test]
    fn placement_site_uses_a_finite_segment_and_footprint_center() {
        let site = WallPlacementSite {
            entrance: [
                PixelPosition { x: 48, y: 48 },
                PixelPosition { x: 80, y: 48 },
            ],
            max_distance_pixels: 0,
        };
        // This center lies on the infinite line, but past the finite segment endpoint.
        assert!(footprint_center_distance_rank(rect(96, 32, 128, 64), site).is_none());
        assert_eq!(
            footprint_center_distance_rank(rect(64, 32, 96, 64), site),
            Some(0)
        );

        let threshold = WallPlacementSite {
            max_distance_pixels: 32,
            ..site
        };
        assert_eq!(
            footprint_center_distance_rank(rect(48, 64, 80, 96), threshold),
            Some(1_024)
        );
        assert!(
            footprint_center_distance_rank(
                rect(48, 64, 80, 96),
                WallPlacementSite {
                    max_distance_pixels: 31,
                    ..site
                }
            )
            .is_none()
        );

        let point_neighborhood = WallPlacementSite {
            entrance: [
                PixelPosition { x: 64, y: 48 },
                PixelPosition { x: 96, y: 48 },
            ],
            max_distance_pixels: 16,
        };
        assert!(footprint_center_distance_rank(rect(32, 32, 96, 64), point_neighborhood).is_some());
        assert!(footprint_center_distance_rank(rect(48, 64, 80, 96), point_neighborhood).is_none());
        assert!(point_in_placement_site(64, 64, point_neighborhood));
        assert!(!point_in_placement_site(64, 65, point_neighborhood));
    }

    #[test]
    fn site_endpoints_keep_standoff_and_clear_every_candidate_for_both_movers() {
        let grid = terrain(16, 12, |_, _| false);
        let mut request = query();
        // This is intentionally the whole routing window. Legacy rectangle exclusion has no room,
        // but the site candidate union leaves the unused, walkable areas connected.
        request.placement_area = request.window;
        request.placement_site = Some(WallPlacementSite {
            entrance: [
                PixelPosition { x: 48, y: 48 },
                PixelPosition { x: 80, y: 48 },
            ],
            max_distance_pixels: 0,
        });
        request.block = CollisionBounds {
            left: 7,
            top: 3,
            right: 11,
            bottom: 5,
        };
        request.allow = Some(CollisionBounds {
            left: 2,
            top: 4,
            right: 13,
            bottom: 1,
        });
        request.buildings = vec![
            BuildingSpec {
                footprint: DepotFootprint {
                    width_tiles: 1,
                    height_tiles: 1,
                },
                collision: CollisionBounds {
                    left: 23,
                    top: 19,
                    right: 14,
                    bottom: 12,
                },
            },
            BuildingSpec {
                footprint: DepotFootprint {
                    width_tiles: 1,
                    height_tiles: 1,
                },
                collision: CollisionBounds {
                    left: 5,
                    top: 14,
                    right: 21,
                    bottom: 2,
                },
            },
        ];
        let entrance = request.placement_site.unwrap().entrance;
        let endpoints = suggest_wall_endpoints(&grid, &[], &request, entrance).unwrap();
        let (candidates, _) = enumerate_candidates(
            &grid,
            &[],
            request.window,
            request.placement_area,
            &request,
            128,
            96,
        )
        .unwrap();
        assert!(!candidates.is_empty());
        for endpoint in endpoints {
            assert!(!point_in_placement_site(
                i64::from(endpoint.x),
                i64::from(endpoint.y),
                request.placement_site.unwrap()
            ));
            for candidate in &candidates {
                for mover in [request.block, request.allow.unwrap()] {
                    assert!(
                        i64::from(endpoint.x)
                            < i64::from(candidate.collision.left) - i64::from(mover.right)
                            || i64::from(endpoint.x)
                                >= i64::from(candidate.collision.right) + i64::from(mover.left)
                            || i64::from(endpoint.y)
                                < i64::from(candidate.collision.top) - i64::from(mover.bottom)
                            || i64::from(endpoint.y)
                                >= i64::from(candidate.collision.bottom) + i64::from(mover.top)
                    );
                }
            }
        }
        request.start = endpoints[0];
        request.end = endpoints[1];
        assert!(WallSearch::new(&grid, &[], request).is_ok());
    }
}
