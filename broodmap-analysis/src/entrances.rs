//! Bounded one-path entrance evidence from terrain cross-sections.
//!
//! The analysis chooses one deterministic clearance-weighted route, profiles fixed-normal
//! cross-sections near its start, and reports only local narrow-to-wide transitions. It is a
//! terrain-model heuristic, not a certified choke, wall, unit-fit, or global connectivity claim.

use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap},
    sync::Arc,
};

use thiserror::Error;

use crate::{
    DIAGONAL_COST, Endpoint, ORTHOGONAL_COST, PixelPosition, Route, TerrainGrid, WalkPosition,
};

const MIN_DISTANCE_PIXELS: u32 = 256;
const MAX_DISTANCE_PIXELS: u32 = 4_096;
const MAX_WIDENING_PERCENT: u16 = 200;
const PROFILE_OFFSETS_PIXELS: [i64; 9] = [-96, -64, -32, 0, 32, 64, 96, 128, 160];
const RAY_CAP_STEPS: u32 = 80;
// Route costs use 1,000 fixed units per 8 logical pixels.
const FIXED_PER_PIXEL: u64 = ORTHOGONAL_COST / 8;
const MIN_WIDTH_FIXED: u64 = 64 * FIXED_PER_PIXEL;
const MAX_WIDTH_FIXED: u64 = 640 * FIXED_PER_PIXEL;
const NMS_DISTANCE_FIXED: u64 = 256 * FIXED_PER_PIXEL;
const MAX_CANDIDATES: usize = 4;
const MAX_BATCH_QUERIES: usize = 256;
const MAX_BATCH_ROUTE_POINTS: usize = 2_097_152;

/// Controls bounded route-local entrance profiling.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct EntranceOptions {
    /// Maximum physical route distance from `start` for candidate centers. Their profile may read
    /// 192 pixels farther for the outward cross-section and tangent check.
    pub max_distance_pixels: u32,
    /// Required outward widening relative to the candidate cross-section width.
    pub min_widening_percent: u16,
}

impl Default for EntranceOptions {
    fn default() -> Self {
        Self {
            max_distance_pixels: 1_024,
            min_widening_percent: 25,
        }
    }
}

/// Error returned while finding route-local entrance evidence.
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum EntranceError {
    #[error(
        "entrance scan distance must be between {MIN_DISTANCE_PIXELS} and {MAX_DISTANCE_PIXELS} pixels, got {max_distance_pixels}"
    )]
    InvalidMaxDistance { max_distance_pixels: u32 },
    #[error(
        "entrance widening percent must be at most {MAX_WIDENING_PERCENT}, got {min_widening_percent}"
    )]
    InvalidMinWideningPercent { min_widening_percent: u16 },
    #[error("entrance batch has {actual} queries, maximum is {MAX_BATCH_QUERIES}")]
    TooManyQueries { actual: usize },
    #[error("entrance batch exceeds {MAX_BATCH_ROUTE_POINTS} returned route points")]
    TooManyRoutePoints,
    #[error("entrance {endpoint} ({position_x}, {position_y}) is outside the grid")]
    EndpointOutOfBounds {
        endpoint: Endpoint,
        position_x: u32,
        position_y: u32,
    },
    #[error("entrance {endpoint} ({position_x}, {position_y}) is blocked")]
    EndpointBlocked {
        endpoint: Endpoint,
        position_x: u32,
        position_y: u32,
    },
}

/// One terrain cross-section that narrows on the approached side and widens outward.
#[derive(Debug, Clone, PartialEq)]
pub struct EntranceCandidate {
    /// Route cell at the candidate cross-section.
    pub position: WalkPosition,
    /// Terrain wall-contact endpoints of the candidate cross-section, in logical pixels.
    pub endpoints: [PixelPosition; 2],
    /// Cross-section span at [`Self::position`], in logical pixels.
    pub width_pixels: f64,
    /// Maximum cross-section width in the 96px approach window.
    pub approach_max_width_pixels: f64,
    /// Minimum cross-section width at the three outward samples (96/128/160px).
    pub outward_min_width_pixels: f64,
    /// True when an outward ray ended at the map edge or ray cap, so its span is a lower bound.
    pub outward_width_is_lower_bound: bool,
    /// Physical arclength from the supplied route start.
    pub distance_from_start_pixels: f64,
}

/// The chosen weighted route and its bounded, de-duplicated entrance evidence.
#[derive(Debug, Clone, PartialEq)]
pub struct EntranceAnalysis {
    /// The deterministic clearance-weighted route, or `None` for disconnected valid endpoints.
    pub route: Option<Route>,
    /// At most four non-maximum-suppressed route-local entrance candidates in route order.
    pub candidates: Vec<EntranceCandidate>,
}

/// Prepared immutable terrain for repeated directed entrance batches.
///
/// The original grid is shared, not copied. Preparation stores one clearance field; route
/// search trees are temporary and released after processing each origin.
#[derive(Debug)]
pub struct EntranceSurvey {
    grid: Arc<TerrainGrid>,
    clearances: Vec<u16>,
}

impl EntranceSurvey {
    /// Computes clearance once while sharing ownership of the immutable grid.
    pub fn new(grid: Arc<TerrainGrid>) -> Self {
        let clearances = grid.clearance().into_radii_pixels();
        Self { grid, clearances }
    }

    /// Returns results in query order, including duplicates. Limits are 256 queries and
    /// 2,097,152 total returned route points; invalid endpoints fail the whole batch.
    /// These are output limits, not a CPU budget: each distinct origin can require a full-grid
    /// Dijkstra search. Repeated destinations share that origin's search; distant destinations
    /// or unreachable targets may exhaust its reachable component.
    pub fn batch(
        &self,
        queries: &[(WalkPosition, WalkPosition)],
        options: &EntranceOptions,
    ) -> Result<Vec<EntranceAnalysis>, EntranceError> {
        let indices = self.grid.validate_entrance_queries(queries, options)?;
        self.grid
            .entrances_batch_prepared(&indices, options, &self.clearances)
    }
}

impl TerrainGrid {
    /// Finds bounded one-sided entrance evidence along one deterministic terrain route.
    ///
    /// The route uses legal 8-neighbor movement and a clearance penalty that favors room centers.
    /// Fixed-normal cross-sections use the current terrain grid, so callers choose whether static
    /// obstacles were rasterized first. The effective candidate window needs 128 pixels behind and
    /// 192 ahead: profile samples span -96 through +160, and each tangent reads another 32 pixels.
    /// Results describe one selected path only and do not certify walls, chokepoints, mover fit,
    /// or alternate approaches.
    pub fn entrances(
        &self,
        start: WalkPosition,
        end: WalkPosition,
        options: &EntranceOptions,
    ) -> Result<EntranceAnalysis, EntranceError> {
        validate_options(options)?;
        let start_index = self.entrance_endpoint_index(start, Endpoint::Start)?;
        let end_index = self.entrance_endpoint_index(end, Endpoint::End)?;
        if start_index == end_index {
            return Ok(EntranceAnalysis {
                route: Some(Route {
                    points: vec![start],
                    distance_pixels: 0.0,
                }),
                candidates: Vec::new(),
            });
        }

        let clearances = self.clearance().into_radii_pixels();
        let Some((route, arclengths)) =
            self.clearance_weighted_route(start_index, end_index, &clearances)
        else {
            return Ok(EntranceAnalysis {
                route: None,
                candidates: Vec::new(),
            });
        };
        let candidates = self.profile_entrances(&route, &arclengths, options);
        Ok(EntranceAnalysis {
            route: Some(route),
            candidates,
        })
    }

    /// Surveys directed queries while sharing one clearance field and one search per origin.
    /// Results match individual calls in input order. Use [`EntranceSurvey`] to retain the
    /// immutable clearance preparation across calls. Batches are limited to 256 queries and
    /// 2,097,152 aggregate returned route points, including duplicated queries. These caps do
    /// not bound runtime: each distinct origin can require a full-grid Dijkstra search.
    pub fn entrances_batch(
        &self,
        queries: &[(WalkPosition, WalkPosition)],
        options: &EntranceOptions,
    ) -> Result<Vec<EntranceAnalysis>, EntranceError> {
        let indices = self.validate_entrance_queries(queries, options)?;
        if indices.is_empty() {
            return Ok(Vec::new());
        }
        let clearances = self.clearance().into_radii_pixels();
        self.entrances_batch_prepared(&indices, options, &clearances)
    }

    fn validate_entrance_queries(
        &self,
        queries: &[(WalkPosition, WalkPosition)],
        options: &EntranceOptions,
    ) -> Result<Vec<(usize, usize)>, EntranceError> {
        validate_options(options)?;
        if queries.len() > MAX_BATCH_QUERIES {
            return Err(EntranceError::TooManyQueries {
                actual: queries.len(),
            });
        }
        queries
            .iter()
            .map(|&(start, end)| {
                Ok((
                    self.entrance_endpoint_index(start, Endpoint::Start)?,
                    self.entrance_endpoint_index(end, Endpoint::End)?,
                ))
            })
            .collect()
    }

    fn entrances_batch_prepared(
        &self,
        queries: &[(usize, usize)],
        options: &EntranceOptions,
        clearances: &[u16],
    ) -> Result<Vec<EntranceAnalysis>, EntranceError> {
        let mut groups = BTreeMap::<usize, BTreeMap<usize, Vec<usize>>>::new();
        for (index, &(start, end)) in queries.iter().enumerate() {
            groups
                .entry(start)
                .or_default()
                .entry(end)
                .or_default()
                .push(index);
        }
        // Unreached targets retain the same disconnected result as an individual search.
        let mut results = vec![
            EntranceAnalysis {
                route: None,
                candidates: Vec::new()
            };
            queries.len()
        ];
        let mut remaining_points = MAX_BATCH_ROUTE_POINTS;
        for (start, mut targets) in groups {
            let mut costs = vec![u64::MAX; self.cells.len()];
            let mut parents = vec![None; self.cells.len()];
            let mut open = BinaryHeap::new();
            costs[start] = 0;
            let position = self.position(start);
            open.push(Reverse((0_u64, position.y, position.x, start)));
            while let Some(Reverse((cost, y, x, current))) = open.pop() {
                if costs[current] != cost {
                    continue;
                }
                if let Some(outputs) = targets.remove(&current) {
                    // Count before allocating a route or duplicate copies. Parent chains use
                    // strictly positive costs and therefore cannot cycle.
                    let mut point_count = 1;
                    let mut cursor = current;
                    while cursor != start {
                        cursor =
                            parents[cursor].expect("settled entrance target has a parent chain");
                        point_count += 1;
                    }
                    let required = point_count * outputs.len();
                    if required > remaining_points {
                        return Err(EntranceError::TooManyRoutePoints);
                    }
                    remaining_points -= required;
                    let (route, arclengths) = self.build_entrance_route(start, current, &parents);
                    let candidates = self.profile_entrances(&route, &arclengths, options);
                    let analysis = EntranceAnalysis {
                        route: Some(route),
                        candidates,
                    };
                    let (&last, earlier) =
                        outputs.split_last().expect("target has at least one query");
                    for &index in earlier {
                        results[index] = analysis.clone();
                    }
                    results[last] = analysis;
                    if targets.is_empty() {
                        break;
                    }
                    // A settled target still expands when another destination remains.
                }
                for (next, step_cost) in self.neighbors(WalkPosition { x, y }) {
                    let next_index = self.index(next).expect("legal neighbor stays in bounds");
                    let radius = u64::from(clearances[current].min(clearances[next_index]));
                    let weighted_step =
                        step_cost.saturating_add(step_cost.saturating_mul(64) / radius);
                    let candidate = cost.saturating_add(weighted_step);
                    if candidate >= costs[next_index] {
                        continue;
                    }
                    costs[next_index] = candidate;
                    parents[next_index] = Some(current);
                    open.push(Reverse((candidate, next.y, next.x, next_index)));
                }
            }
        }
        Ok(results)
    }

    fn entrance_endpoint_index(
        &self,
        position: WalkPosition,
        endpoint: Endpoint,
    ) -> Result<usize, EntranceError> {
        let index = self
            .index(position)
            .ok_or(EntranceError::EndpointOutOfBounds {
                endpoint,
                position_x: position.x,
                position_y: position.y,
            })?;
        if !self.cells[index].walkable {
            return Err(EntranceError::EndpointBlocked {
                endpoint,
                position_x: position.x,
                position_y: position.y,
            });
        }
        Ok(index)
    }

    fn clearance_weighted_route(
        &self,
        start: usize,
        end: usize,
        clearances: &[u16],
    ) -> Option<(Route, Vec<u64>)> {
        let mut costs = vec![u64::MAX; self.cells.len()];
        let mut parents = vec![None; self.cells.len()];
        let mut open = BinaryHeap::new();
        costs[start] = 0;
        let start_position = self.position(start);
        open.push(Reverse((0_u64, start_position.y, start_position.x, start)));

        while let Some(Reverse((cost, y, x, current))) = open.pop() {
            if costs[current] != cost {
                continue;
            }
            if current == end {
                return Some(self.build_entrance_route(start, end, &parents));
            }
            let position = WalkPosition { x, y };
            for (next, step_cost) in self.neighbors(position) {
                let next_index = self
                    .index(next)
                    .expect("TerrainGrid neighbor must remain in bounds");
                let radius = u64::from(clearances[current].min(clearances[next_index]));
                let weighted_step = step_cost.saturating_add(step_cost.saturating_mul(64) / radius);
                let candidate = cost.saturating_add(weighted_step);
                if candidate >= costs[next_index] {
                    continue;
                }
                costs[next_index] = candidate;
                parents[next_index] = Some(current);
                open.push(Reverse((candidate, next.y, next.x, next_index)));
            }
        }
        None
    }

    fn build_entrance_route(
        &self,
        start: usize,
        end: usize,
        parents: &[Option<usize>],
    ) -> (Route, Vec<u64>) {
        let mut indices = Vec::new();
        let mut current = end;
        for _ in 0..=self.cells.len() {
            indices.push(current);
            if current == start {
                break;
            }
            current = parents[current].unwrap_or(start);
        }
        indices.reverse();
        let mut arclengths = Vec::with_capacity(indices.len());
        let mut physical_cost = 0_u64;
        for (offset, &index) in indices.iter().enumerate() {
            if offset != 0 {
                physical_cost = physical_cost.saturating_add(step_cost(
                    self.position(indices[offset - 1]),
                    self.position(index),
                ));
            }
            arclengths.push(physical_cost);
        }
        let points: Vec<_> = indices
            .into_iter()
            .map(|index| self.position(index))
            .collect();
        (
            Route {
                points,
                distance_pixels: fixed_to_pixels(physical_cost),
            },
            arclengths,
        )
    }

    fn profile_entrances(
        &self,
        route: &Route,
        arclengths: &[u64],
        options: &EntranceOptions,
    ) -> Vec<EntranceCandidate> {
        let total = *arclengths.last().unwrap_or(&0);
        let max_distance = u64::from(options.max_distance_pixels) * FIXED_PER_PIXEL;
        let mut raw = Vec::new();
        for (route_index, &distance) in arclengths.iter().enumerate() {
            if distance > max_distance {
                break;
            }
            if distance < 96 * FIXED_PER_PIXEL
                || total.saturating_sub(distance) < 160 * FIXED_PER_PIXEL
            {
                continue;
            }
            let Some(sample_indices) = profile_sample_indices(arclengths, route_index) else {
                continue;
            };
            let Some(tangent) = route_tangent(route, arclengths, route_index, 64) else {
                continue;
            };
            let normal = (-tangent.1, tangent.0);
            if !profile_tangents_are_smooth(route, arclengths, &sample_indices, tangent) {
                continue;
            }
            // +32/+64 still participate in tangent smoothness above, but their widths
            // do not feed any filter. Cast only the four inner and three outer sections.
            let sections = [0, 1, 2, 3, 6, 7, 8]
                .map(|slot| self.cross_section(route.points[sample_indices[slot]], normal));
            let candidate = &sections[3];
            // The approach samples and candidate itself must contact actual terrain blockers.
            // The positive samples are the widening evidence, so they may already lie in an
            // unbounded room or reach the ray cap.
            if !sections[..4]
                .iter()
                .all(CrossSection::actual_blockers_both_sides)
            {
                continue;
            }
            if !(MIN_WIDTH_FIXED..=MAX_WIDTH_FIXED).contains(&candidate.width_fixed) {
                continue;
            }
            let approach_max = sections[..3]
                .iter()
                .map(|section| section.width_fixed)
                .max()
                .unwrap_or(candidate.width_fixed);
            if approach_max.saturating_mul(100) > candidate.width_fixed.saturating_mul(130) {
                continue;
            }
            let outward = &sections[4..];
            let outward_min = outward
                .iter()
                .map(|section| section.width_fixed)
                .min()
                .unwrap_or(candidate.width_fixed);
            if outward_min < candidate.width_fixed.saturating_add(64 * FIXED_PER_PIXEL)
                || outward_min.saturating_mul(100)
                    < candidate
                        .width_fixed
                        .saturating_mul(u64::from(100 + options.min_widening_percent))
            {
                continue;
            }
            raw.push(RawCandidate {
                route_index,
                arclength_fixed: distance,
                width_fixed: candidate.width_fixed,
                outward_min_fixed: outward_min,
                candidate: EntranceCandidate {
                    position: route.points[route_index],
                    endpoints: candidate.endpoints,
                    width_pixels: fixed_to_pixels(candidate.width_fixed),
                    approach_max_width_pixels: fixed_to_pixels(approach_max),
                    outward_min_width_pixels: fixed_to_pixels(outward_min),
                    outward_width_is_lower_bound: outward.iter().any(CrossSection::is_lower_bound),
                    distance_from_start_pixels: fixed_to_pixels(distance),
                },
            });
        }
        non_maximum_suppression(raw)
    }

    fn cross_section(&self, center: WalkPosition, normal: (i32, i32)) -> CrossSection {
        let negative = self.raymarch(center, (-normal.0, -normal.1));
        let positive = self.raymarch(center, normal);
        CrossSection {
            width_fixed: negative.length_fixed.saturating_add(positive.length_fixed),
            endpoints: [negative.endpoint, positive.endpoint],
            negative,
            positive,
        }
    }

    fn raymarch(&self, center: WalkPosition, direction: (i32, i32)) -> RayMarch {
        let diagonal = direction.0 != 0 && direction.1 != 0;
        let direction_cost = if diagonal {
            DIAGONAL_COST
        } else {
            ORTHOGONAL_COST
        };
        let mut current = center;
        for steps in 0..RAY_CAP_STEPS {
            let Some(next) = offset(current, direction) else {
                return RayMarch::map_edge(current, steps, direction, direction_cost);
            };
            let Some(next_index) = self.index(next) else {
                return RayMarch::map_edge(current, steps, direction, direction_cost);
            };
            if !self.cells[next_index].walkable {
                return RayMarch::blocker(current, steps, direction, direction_cost);
            }
            if diagonal {
                let side_a = offset(current, (direction.0, 0));
                let side_b = offset(current, (0, direction.1));
                let side_a_blocked = side_a
                    .and_then(|position| self.index(position))
                    .is_none_or(|index| !self.cells[index].walkable);
                let side_b_blocked = side_b
                    .and_then(|position| self.index(position))
                    .is_none_or(|index| !self.cells[index].walkable);
                if side_a_blocked || side_b_blocked {
                    // Both side cells combine in-bounds coordinates from current and next,
                    // so this is a terrain blocker rather than a map-edge contact.
                    return RayMarch::blocker(current, steps, direction, direction_cost);
                }
            }
            current = next;
        }
        RayMarch::capped(current, RAY_CAP_STEPS, direction, direction_cost)
    }
}

fn validate_options(options: &EntranceOptions) -> Result<(), EntranceError> {
    if !(MIN_DISTANCE_PIXELS..=MAX_DISTANCE_PIXELS).contains(&options.max_distance_pixels) {
        return Err(EntranceError::InvalidMaxDistance {
            max_distance_pixels: options.max_distance_pixels,
        });
    }
    if options.min_widening_percent > MAX_WIDENING_PERCENT {
        return Err(EntranceError::InvalidMinWideningPercent {
            min_widening_percent: options.min_widening_percent,
        });
    }
    Ok(())
}

fn step_cost(left: WalkPosition, right: WalkPosition) -> u64 {
    if left.x != right.x && left.y != right.y {
        DIAGONAL_COST
    } else {
        ORTHOGONAL_COST
    }
}

fn fixed_to_pixels(value: u64) -> f64 {
    value as f64 * 8.0 / ORTHOGONAL_COST as f64
}

fn pixels_to_fixed(value: u64) -> u64 {
    value.saturating_mul(FIXED_PER_PIXEL)
}

fn offset(position: WalkPosition, direction: (i32, i32)) -> Option<WalkPosition> {
    Some(WalkPosition {
        x: position.x.checked_add_signed(direction.0)?,
        y: position.y.checked_add_signed(direction.1)?,
    })
}

fn center_endpoint(last: WalkPosition, direction: (i32, i32)) -> PixelPosition {
    let x = i64::from(last.x) * 8 + 4 + i64::from(direction.0) * 4;
    let y = i64::from(last.y) * 8 + 4 + i64::from(direction.1) * 4;
    PixelPosition {
        x: u32::try_from(x).expect("ray endpoints stay on a validated map edge"),
        y: u32::try_from(y).expect("ray endpoints stay on a validated map edge"),
    }
}

fn profile_sample_indices(arclengths: &[u64], route_index: usize) -> Option<[usize; 9]> {
    let distance = *arclengths.get(route_index)?;
    let mut result = [0; 9];
    for (slot, offset_pixels) in PROFILE_OFFSETS_PIXELS.iter().enumerate() {
        let target = if *offset_pixels < 0 {
            distance.checked_sub(pixels_to_fixed(offset_pixels.unsigned_abs()))?
        } else {
            distance.checked_add(pixels_to_fixed(*offset_pixels as u64))?
        };
        let index = arclengths.partition_point(|&sample| sample < target);
        *result.get_mut(slot)? = (index < arclengths.len()).then_some(index)?;
    }
    Some(result)
}

fn route_tangent(
    route: &Route,
    arclengths: &[u64],
    route_index: usize,
    offset_pixels: u64,
) -> Option<(i32, i32)> {
    let distance = *arclengths.get(route_index)?;
    let before_target = distance.checked_sub(pixels_to_fixed(offset_pixels))?;
    let after_target = distance.checked_add(pixels_to_fixed(offset_pixels))?;
    let before = arclengths.partition_point(|&sample| sample < before_target);
    let after = arclengths.partition_point(|&sample| sample < after_target);
    let before = route.points.get(before)?;
    let after = route.points.get(after)?;
    quantize_tangent(
        i64::from(after.x) - i64::from(before.x),
        i64::from(after.y) - i64::from(before.y),
    )
}

fn quantize_tangent(delta_x: i64, delta_y: i64) -> Option<(i32, i32)> {
    if delta_x == 0 && delta_y == 0 {
        return None;
    }
    let sign_x = if delta_x < 0 { -1 } else { 1 };
    let sign_y = if delta_y < 0 { -1 } else { 1 };
    let absolute_x = delta_x.unsigned_abs();
    let absolute_y = delta_y.unsigned_abs();
    if absolute_y.saturating_mul(2) < absolute_x {
        Some((sign_x, 0))
    } else if absolute_x.saturating_mul(2) < absolute_y {
        Some((0, sign_y))
    } else {
        Some((sign_x, sign_y))
    }
}

fn profile_tangents_are_smooth(
    route: &Route,
    arclengths: &[u64],
    samples: &[usize; 9],
    tangent: (i32, i32),
) -> bool {
    samples.iter().all(|&index| {
        let Some(sample_tangent) = route_tangent(route, arclengths, index, 32) else {
            return false;
        };
        let dot = tangent.0 * sample_tangent.0 + tangent.1 * sample_tangent.1;
        let cross = tangent.0 * sample_tangent.1 - tangent.1 * sample_tangent.0;
        dot > 0 && cross.abs() <= dot
    })
}

#[derive(Debug, Copy, Clone)]
struct RayMarch {
    length_fixed: u64,
    endpoint: PixelPosition,
    actual_blocker: bool,
    lower_bound: bool,
}

impl RayMarch {
    fn blocker(last: WalkPosition, steps: u32, direction: (i32, i32), direction_cost: u64) -> Self {
        Self {
            length_fixed: (u64::from(steps) * 2 + 1).saturating_mul(direction_cost) / 2,
            endpoint: center_endpoint(last, direction),
            actual_blocker: true,
            lower_bound: false,
        }
    }

    fn map_edge(
        last: WalkPosition,
        steps: u32,
        direction: (i32, i32),
        direction_cost: u64,
    ) -> Self {
        Self {
            length_fixed: (u64::from(steps) * 2 + 1).saturating_mul(direction_cost) / 2,
            endpoint: center_endpoint(last, direction),
            actual_blocker: false,
            lower_bound: true,
        }
    }

    fn capped(last: WalkPosition, steps: u32, direction: (i32, i32), direction_cost: u64) -> Self {
        Self {
            length_fixed: (u64::from(steps) * 2 + 1).saturating_mul(direction_cost) / 2,
            endpoint: center_endpoint(last, direction),
            actual_blocker: false,
            lower_bound: true,
        }
    }
}

#[derive(Debug, Clone)]
struct CrossSection {
    width_fixed: u64,
    endpoints: [PixelPosition; 2],
    negative: RayMarch,
    positive: RayMarch,
}

impl CrossSection {
    fn actual_blockers_both_sides(&self) -> bool {
        self.negative.actual_blocker && self.positive.actual_blocker
    }

    fn is_lower_bound(&self) -> bool {
        self.negative.lower_bound || self.positive.lower_bound
    }
}

#[derive(Debug, Clone)]
struct RawCandidate {
    route_index: usize,
    arclength_fixed: u64,
    width_fixed: u64,
    outward_min_fixed: u64,
    candidate: EntranceCandidate,
}

fn non_maximum_suppression(mut raw: Vec<RawCandidate>) -> Vec<EntranceCandidate> {
    raw.sort_unstable_by(|left, right| {
        left.width_fixed
            .cmp(&right.width_fixed)
            .then_with(|| {
                let left_ratio = u128::from(left.outward_min_fixed) * u128::from(right.width_fixed);
                let right_ratio =
                    u128::from(right.outward_min_fixed) * u128::from(left.width_fixed);
                right_ratio.cmp(&left_ratio)
            })
            .then_with(|| right.route_index.cmp(&left.route_index))
    });
    let mut selected = Vec::new();
    for candidate in raw {
        if selected.iter().any(|accepted: &RawCandidate| {
            accepted.arclength_fixed.abs_diff(candidate.arclength_fixed) < NMS_DISTANCE_FIXED
        }) {
            continue;
        }
        selected.push(candidate);
        if selected.len() == MAX_CANDIDATES {
            break;
        }
    }
    selected.sort_unstable_by_key(|candidate| candidate.route_index);
    selected
        .into_iter()
        .map(|candidate| candidate.candidate)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerrainCell;

    fn grid(width: u32, height: u32, walkable: impl Fn(u32, u32) -> bool) -> TerrainGrid {
        let mut cells = Vec::with_capacity((width * height) as usize);
        for y in 0..height {
            for x in 0..width {
                cells.push(TerrainCell {
                    walkable: walkable(x, y),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                });
            }
        }
        TerrainGrid::from_cells(width, height, cells).unwrap()
    }

    fn point(x: u32, y: u32) -> WalkPosition {
        WalkPosition { x, y }
    }

    #[test]
    fn cardinal_and_diagonal_cross_sections_have_exact_fixed_spans_and_endpoints() {
        let interior = grid(9, 9, |x, y| (1..=7).contains(&x) && (1..=7).contains(&y));
        let cardinal = interior.cross_section(point(4, 4), (0, 1));
        assert_eq!(cardinal.width_fixed, 7_000);
        assert_eq!(fixed_to_pixels(cardinal.width_fixed), 56.0);
        assert_eq!(
            cardinal.endpoints,
            [
                PixelPosition { x: 36, y: 8 },
                PixelPosition { x: 36, y: 64 }
            ]
        );
        assert!(cardinal.actual_blockers_both_sides());

        let diagonal = interior.cross_section(point(4, 4), (1, 1));
        assert_eq!(diagonal.width_fixed, 9_898);
        assert!((fixed_to_pixels(diagonal.width_fixed) - 79.184).abs() < 0.001);
        assert_eq!(
            diagonal.endpoints,
            [PixelPosition { x: 8, y: 8 }, PixelPosition { x: 64, y: 64 }]
        );
        assert!(diagonal.actual_blockers_both_sides());

        let diagonal_corner = grid(9, 9, |x, y| !(x == 5 && y == 4));
        let stopped = diagonal_corner.raymarch(point(4, 4), (1, 1));
        assert!(
            stopped.actual_blocker,
            "blocked diagonal side prevents corner cutting"
        );
        assert_eq!(stopped.length_fixed, 707);
        assert_eq!(stopped.endpoint, PixelPosition { x: 40, y: 40 });
    }

    #[test]
    fn ray_cap_and_map_edge_are_explicit_width_lower_bounds() {
        let open = grid(201, 201, |_, _| true);
        let capped = open.cross_section(point(100, 100), (0, 1));
        assert_eq!(capped.width_fixed, 161_000);
        assert!(capped.is_lower_bound());
        assert!(!capped.actual_blockers_both_sides());

        let edge = grid(9, 9, |_, _| true).cross_section(point(4, 4), (0, 1));
        assert_eq!(edge.width_fixed, 9_000);
        assert!(edge.is_lower_bound());
        assert_eq!(
            edge.endpoints,
            [
                PixelPosition { x: 36, y: 0 },
                PixelPosition { x: 36, y: 72 }
            ]
        );
    }

    #[test]
    fn invalid_options_and_endpoints_are_explicit() {
        let terrain = grid(4, 4, |x, y| x != 0 || y != 0);
        assert_eq!(
            terrain.entrances(point(4, 0), point(1, 1), &EntranceOptions::default(),),
            Err(EntranceError::EndpointOutOfBounds {
                endpoint: Endpoint::Start,
                position_x: 4,
                position_y: 0,
            })
        );
        assert_eq!(
            terrain.entrances(point(0, 0), point(1, 1), &EntranceOptions::default(),),
            Err(EntranceError::EndpointBlocked {
                endpoint: Endpoint::Start,
                position_x: 0,
                position_y: 0,
            })
        );
        assert_eq!(
            terrain.entrances(
                point(1, 1),
                point(1, 1),
                &EntranceOptions {
                    max_distance_pixels: 255,
                    ..EntranceOptions::default()
                },
            ),
            Err(EntranceError::InvalidMaxDistance {
                max_distance_pixels: 255,
            })
        );
        assert_eq!(
            terrain.entrances(
                point(1, 1),
                point(1, 1),
                &EntranceOptions {
                    min_widening_percent: 201,
                    ..EntranceOptions::default()
                },
            ),
            Err(EntranceError::InvalidMinWideningPercent {
                min_widening_percent: 201,
            })
        );
    }

    #[test]
    fn disconnected_and_equal_endpoints_are_not_candidates() {
        let terrain = grid(8, 4, |x, _| x != 3 && x != 4);
        let disconnected = terrain
            .entrances(point(1, 1), point(6, 1), &EntranceOptions::default())
            .unwrap();
        assert!(disconnected.route.is_none());
        assert!(disconnected.candidates.is_empty());
        let diagonal_pinch = grid(2, 2, |x, y| matches!((x, y), (0, 0) | (1, 1)));
        let pinched = diagonal_pinch
            .entrances(point(0, 0), point(1, 1), &EntranceOptions::default())
            .unwrap();
        assert!(
            pinched.route.is_none(),
            "a diagonal blocked-corner pinch is disconnected"
        );
        let same = terrain
            .entrances(point(1, 1), point(1, 1), &EntranceOptions::default())
            .unwrap();
        assert_eq!(same.route.unwrap().points, vec![point(1, 1)]);
        assert!(same.candidates.is_empty());
    }

    fn assert_batch_matches_singles(
        terrain: TerrainGrid,
        queries: &[(WalkPosition, WalkPosition)],
    ) {
        let options = EntranceOptions::default();
        let expected: Vec<_> = queries
            .iter()
            .map(|&(start, end)| terrain.entrances(start, end, &options).unwrap())
            .collect();
        assert_eq!(
            terrain.entrances_batch(queries, &options).unwrap(),
            expected
        );
        let terrain = Arc::new(terrain);
        let prepared = EntranceSurvey::new(Arc::clone(&terrain));
        drop(terrain);
        assert_eq!(prepared.batch(queries, &options).unwrap(), expected);
        let reversed: Vec<_> = queries.iter().copied().rev().collect();
        let reversed_expected: Vec<_> = expected.into_iter().rev().collect();
        assert_eq!(
            prepared.batch(&reversed, &options).unwrap(),
            reversed_expected
        );
    }

    #[test]
    fn batches_preserve_routes_candidates_and_continue_through_targets() {
        let a = point(10, 44);
        let b = point(30, 44);
        let c = point(80, 44);
        let terrain = hallway_to_room();
        assert!(
            !terrain
                .entrances(a, c, &EntranceOptions::default())
                .unwrap()
                .candidates
                .is_empty()
        );
        assert_batch_matches_singles(terrain, &[(a, b), (a, c), (c, a), (a, a), (a, c)]);
        // One-cell corridor makes expansion through an already settled target necessary.
        assert_batch_matches_singles(
            grid(20, 3, |_, y| y == 1),
            &[(point(0, 1), point(4, 1)), (point(0, 1), point(19, 1))],
        );
    }

    #[test]
    fn batches_match_individual_searches_on_random_and_disconnected_grids() {
        let state = std::cell::Cell::new(0x72ab_341d_u32);
        for _ in 0..64 {
            let terrain = grid(17, 13, |x, y| {
                state.set(
                    state
                        .get()
                        .wrapping_mul(1_664_525)
                        .wrapping_add(1_013_904_223),
                );
                (x == 1 && y == 1) || (x == 15 && y == 11) || !state.get().is_multiple_of(5)
            });
            let a = point(1, 1);
            let b = point(15, 11);
            assert_batch_matches_singles(terrain, &[(a, b), (b, a), (a, a), (a, b)]);
        }
        let a = point(1, 1);
        let b = point(6, 1);
        assert_batch_matches_singles(
            grid(8, 4, |x, _| x != 3),
            &[(a, b), (a, point(2, 2)), (b, a), (a, a), (a, b)],
        );
    }

    #[test]
    fn batch_limits_and_validation_preserve_input_order() {
        let terrain = grid(4, 4, |x, y| x != 0 || y != 0);
        let options = EntranceOptions::default();
        assert!(terrain.entrances_batch(&[], &options).unwrap().is_empty());
        assert_eq!(
            terrain.entrances_batch(&vec![(point(1, 1), point(1, 1)); 257], &options),
            Err(EntranceError::TooManyQueries { actual: 257 })
        );
        let queries = [(point(2, 2), point(0, 0)), (point(1, 1), point(9, 9))];
        assert_eq!(
            terrain.entrances_batch(&queries, &options),
            Err(EntranceError::EndpointBlocked {
                endpoint: Endpoint::End,
                position_x: 0,
                position_y: 0,
            })
        );
        let invalid = EntranceOptions {
            max_distance_pixels: 0,
            ..options
        };
        assert_eq!(
            terrain.entrances_batch(&[], &invalid),
            Err(EntranceError::InvalidMaxDistance {
                max_distance_pixels: 0
            })
        );
        // Nine full rows joined by alternating one-cell connectors yield >8192 points.
        let snake = grid(1024, 17, |x, y| {
            y % 2 == 0 || x == if y % 4 == 1 { 1023 } else { 0 }
        });
        let query = (point(0, 0), point(1023, 16));
        assert!(
            snake.entrances_batch(&[query], &options).unwrap()[0]
                .route
                .as_ref()
                .unwrap()
                .points
                .len()
                > 8192
        );
        assert_eq!(
            snake.entrances_batch(&vec![query; 256], &options),
            Err(EntranceError::TooManyRoutePoints)
        );
    }

    fn hallway_to_room() -> TerrainGrid {
        grid(100, 90, |x, y| {
            let hallway = (1..=40).contains(&x) && (37..=52).contains(&y);
            let room = (41..=98).contains(&x) && (5..=84).contains(&y);
            hallway || room
        })
    }

    #[test]
    fn hallway_opening_into_room_is_route_local_entrance_evidence() {
        let terrain = hallway_to_room();
        let analysis = terrain
            .entrances(
                point(10, 44),
                point(80, 44),
                &EntranceOptions {
                    max_distance_pixels: 1_024,
                    min_widening_percent: 25,
                },
            )
            .unwrap();
        let route = analysis
            .route
            .as_ref()
            .expect("walkable endpoints must connect");
        assert!(route.points.windows(2).all(|pair| {
            terrain
                .neighbors(pair[0])
                .any(|(neighbor, _)| neighbor == pair[1])
        }));
        let candidate = analysis
            .candidates
            .first()
            .expect("hallway must open into broad room");
        assert_eq!(
            candidate.position.x, 40,
            "latest confined cross-section is the mouth"
        );
        assert!((96.0..=160.0).contains(&candidate.width_pixels));
        assert!(candidate.outward_min_width_pixels >= candidate.width_pixels + 64.0);
        assert!(
            candidate
                .endpoints
                .iter()
                .all(|endpoint| endpoint.y > 0 && endpoint.y < terrain.height() * 8)
        );
    }

    #[test]
    fn qualifying_mouth_marks_capped_outward_width_as_a_lower_bound() {
        let terrain = grid(100, 220, |x, y| {
            let hallway = (1..=40).contains(&x) && (105..=120).contains(&y);
            let room = (41..=98).contains(&x) && (1..=218).contains(&y);
            hallway || room
        });
        let analysis = terrain
            .entrances(point(10, 112), point(80, 112), &EntranceOptions::default())
            .unwrap();
        let candidate = analysis
            .candidates
            .iter()
            .find(|candidate| candidate.position.x == 40)
            .expect("capped-room fixture must retain the mouth");
        assert!(candidate.outward_width_is_lower_bound);
        assert!(candidate.outward_min_width_pixels > 1_000.0);
    }

    #[test]
    fn flat_corridor_notch_and_bend_do_not_invent_entrances() {
        let flat = grid(112, 24, |x, y| {
            (1..=110).contains(&x) && (8..=16).contains(&y)
        });
        assert!(
            flat.entrances(point(8, 12), point(100, 12), &EntranceOptions::default())
                .unwrap()
                .candidates
                .is_empty()
        );

        let notch = grid(112, 48, |x, y| {
            let hallway = (1..=110).contains(&x) && (18..=28).contains(&y);
            let rock = (52..=54).contains(&x) && (18..=20).contains(&y);
            hallway && !rock
        });
        assert!(
            notch
                .entrances(point(8, 23), point(100, 23), &EntranceOptions::default())
                .unwrap()
                .candidates
                .is_empty()
        );

        let bend = grid(80, 80, |x, y| {
            let horizontal = (1..=40).contains(&x) && (32..=42).contains(&y);
            let vertical = (32..=42).contains(&x) && (1..=70).contains(&y);
            horizontal || vertical
        });
        assert!(
            bend.entrances(point(8, 37), point(37, 64), &EntranceOptions::default())
                .unwrap()
                .candidates
                .is_empty()
        );
    }

    #[test]
    fn diagonal_legal_rays_and_max_grid_remain_bounded_and_deterministic() {
        let diagonal = grid(72, 72, |x, y| {
            let along = x.abs_diff(y) <= 5 && (2..=69).contains(&x) && (2..=69).contains(&y);
            let room = (40..=69).contains(&x) && (40..=69).contains(&y);
            along || room
        });
        let first = diagonal
            .entrances(point(8, 8), point(60, 60), &EntranceOptions::default())
            .unwrap();
        let second = diagonal
            .entrances(point(8, 8), point(60, 60), &EntranceOptions::default())
            .unwrap();
        assert_eq!(first, second);
        for candidate in &first.candidates {
            assert!(candidate.width_pixels.is_finite());
            assert!(candidate.distance_from_start_pixels <= 1_024.0);
        }

        let max = grid(1_024, 1_024, |_, _| true);
        let analysis = max
            .entrances(
                point(1, 1),
                point(1_022, 1_022),
                &EntranceOptions::default(),
            )
            .unwrap();
        assert!(analysis.route.is_some());
        assert!(
            analysis.candidates.is_empty(),
            "open terrain has no actual inner wall contacts"
        );
    }
}
