//! Deterministic structural ramp boundaries from terrain flags.
//!
//! This is deliberately independent of route-local entrance evidence. A ramp boundary is emitted
//! only when the terrain's walkable ramp cells describe one unambiguous adjacent-elevation slope
//! and both of its longitudinal ends have a finite wall-to-wall cross-section. A flagged end may
//! move through a short, legal run of same-elevation, unbuildable cells only when that run reaches
//! a same-elevation buildable landing. This finds a local opening without turning an adjacent apron
//! or room chord into a ramp boundary.

use crate::{PixelPosition, TerrainGrid, WalkPosition};

const PIXELS_PER_WALK_CELL: f64 = 8.0;
const FIXED_PER_WALK_CELL: f64 = 1_000.0;
const END_BAND_CELLS: i64 = 2;
const MIN_LONGITUDINAL_EXTENT_CELLS: i64 = 6;
const MIN_END_SEPARATION_CELLS: i64 = 4;
const MIN_COMPONENT_CELLS: usize = 12;
const MAX_LANDING_STEPS: u32 = 16;
const INITIAL_SHOULDER_DISTANCE: i64 = 8;
const MIN_OPENING_INCREASE_FIXED: u64 = 8_000;
const MAX_LOCAL_OPENING_FIXED: u64 = 48_000;

/// One finite, terrain-bounded cross-section at an end of a structural ramp.
#[derive(Debug, Clone, PartialEq)]
pub struct RampEnd {
    /// The 8x8-pixel walk cell used as the cross-section center.
    pub position: WalkPosition,
    /// Terrain-wall contacts of the full, unclipped cross-section.
    pub endpoints: [PixelPosition; 2],
    /// Cross-section length in pixels.
    pub width_pixels: f64,
}

/// Two terrain-bounded ends of one flagged ramp component.
///
/// `lower` and `upper` are ordered along the elevation-centroid ascent direction. They are not
/// route directions and do not claim unit fit, a wall, or global connectivity.
#[derive(Debug, Clone, PartialEq)]
pub struct Ramp {
    /// One-based row-major ordinal among emitted, unambiguous components.
    pub id: u32,
    /// The lower of the two adjacent terrain elevation values observed at the component.
    pub lower_elevation: u8,
    /// The upper of the two adjacent terrain elevation values observed at the component.
    pub upper_elevation: u8,
    /// The finite cross-section at the lower longitudinal end.
    pub lower: RampEnd,
    /// The finite cross-section at the upper longitudinal end.
    pub upper: RampEnd,
    /// Number of walkable ramp-flagged cells in this component.
    pub cell_count: u32,
}

#[derive(Debug, Copy, Clone)]
struct EndCandidate {
    position: WalkPosition,
    longitudinal: i64,
    width_fixed: u64,
    endpoints: [PixelPosition; 2],
}

#[derive(Debug, Copy, Clone)]
struct EndSearch {
    axis: (i32, i32),
    normal: (i32, i32),
    extreme: i64,
    transverse_sum: i64,
    lower: bool,
}

impl TerrainGrid {
    /// Finds independent, two-ended terrain ramp boundaries in deterministic row-major order.
    ///
    /// Components use legal 8-neighbor movement: diagonal neighbors connect only when both
    /// cardinal side cells are walkable. Each component adds its immediate legal walkable border
    /// to its elevation samples, then requires exactly two adjacent elevations and at least one
    /// direct transition between them. The ascent direction is the quantized vector from the low
    /// elevation centroid to the high elevation centroid.
    ///
    /// Component cells are projected onto that ascent axis and its perpendicular. At each end,
    /// this examines only cells in a two-projection-unit longitudinal band, chooses the most central
    /// finite section, and reports its full terrain-wall contacts. Map-edge and ray-cap contacts
    /// are rejected by the shared wall-ray sampler. Integer projections use `x * axis.x + y * axis.y`
    /// for an eight-way unit heading: one diagonal step changes the longitudinal projection by two.
    /// Components need 12 cells and longitudinal extent six; selected ends need separation four.
    /// If an initial section lies on an apron, this looks up to eight projection units inward for
    /// the first central finite section at least 64 pixels and 25% narrower. Centrality is chosen
    /// before testing width, and the search reserves the required separation between the ends.
    /// For an unbuildable selected end, a legal same-elevation extension is inspected for at most
    /// 16 cells and is accepted only after a same-elevation buildable landing. The first finite
    /// local opening is retained when it is at least 64 pixels and 75% wider than the flagged
    /// section, while still no more than 384 pixels and four times its width. An excessive or
    /// unbounded opening instead leaves the last narrow finite section; no proven landing retains
    /// the flagged end. Widths use the same fixed 1000-per-8-pixel units as the route and entrance
    /// code.
    pub fn ramps(&self) -> Vec<Ramp> {
        let cell_count = self.cells.len();
        let mut visited = vec![false; cell_count];
        let mut sample_marks = vec![0_u32; cell_count];
        let mut sample_epoch = 0_u32;
        let mut ramps = Vec::new();

        for start_index in 0..cell_count {
            if visited[start_index] || !self.is_walkable_ramp_index(start_index) {
                continue;
            }
            let start = self.position(start_index);
            let component = self.ramp_component(start, &mut visited);
            if component.len() < MIN_COMPONENT_CELLS {
                continue;
            }

            sample_epoch = sample_epoch.wrapping_add(1);
            if sample_epoch == 0 {
                sample_marks.fill(0);
                sample_epoch = 1;
            }
            let mut elevation_counts = [0_u64; 3];
            let mut elevation_sums = [(0_i64, 0_i64); 3];
            for &position in &component {
                self.add_elevation_sample(
                    position,
                    sample_epoch,
                    &mut sample_marks,
                    &mut elevation_counts,
                    &mut elevation_sums,
                );
                for (neighbor, _) in self.neighbors(position) {
                    self.add_elevation_sample(
                        neighbor,
                        sample_epoch,
                        &mut sample_marks,
                        &mut elevation_counts,
                        &mut elevation_sums,
                    );
                }
            }
            let present: Vec<_> = elevation_counts
                .iter()
                .enumerate()
                .filter_map(|(elevation, &count)| (count != 0).then_some(elevation as u8))
                .collect();
            let [lower_elevation, upper_elevation] = present.as_slice() else {
                continue;
            };
            if *upper_elevation != lower_elevation.saturating_add(1)
                || !self.has_adjacent_elevation_transition(&component)
            {
                continue;
            }

            let lower_index = usize::from(*lower_elevation);
            let upper_index = usize::from(*upper_elevation);
            let axis = quantized_centroid_direction(
                elevation_sums[lower_index],
                elevation_counts[lower_index],
                elevation_sums[upper_index],
                elevation_counts[upper_index],
            );
            let Some(axis) = axis else {
                continue;
            };
            let normal = (-axis.1, axis.0);
            let mut minimum = i64::MAX;
            let mut maximum = i64::MIN;
            let mut transverse_sum = 0_i64;
            for &position in &component {
                let longitudinal = project(position, axis);
                minimum = minimum.min(longitudinal);
                maximum = maximum.max(longitudinal);
                transverse_sum += project(position, normal);
            }
            if maximum.saturating_sub(minimum) < MIN_LONGITUDINAL_EXTENT_CELLS {
                continue;
            }

            let lower = self.select_end(&component, axis, normal, minimum, transverse_sum, true);
            let upper = self.select_end(&component, axis, normal, maximum, transverse_sum, false);
            let (Some(lower), Some(upper)) = (lower, upper) else {
                continue;
            };
            let lower = self.initial_shoulder(
                &component,
                EndSearch {
                    axis,
                    normal,
                    extreme: minimum,
                    transverse_sum,
                    lower: true,
                },
                lower,
            );
            let upper = self.initial_shoulder(
                &component,
                EndSearch {
                    axis,
                    normal,
                    extreme: maximum,
                    transverse_sum,
                    lower: false,
                },
                upper,
            );
            let lower = self.refine_end(lower, axis, normal, (-axis.0, -axis.1));
            let upper = self.refine_end(upper, axis, normal, axis);
            if upper.longitudinal.saturating_sub(lower.longitudinal) < MIN_END_SEPARATION_CELLS
                || canonical_endpoints(lower.endpoints) == canonical_endpoints(upper.endpoints)
            {
                continue;
            }

            let id = u32::try_from(ramps.len() + 1).expect("grid component count is bounded");
            ramps.push(Ramp {
                id,
                lower_elevation: *lower_elevation,
                upper_elevation: *upper_elevation,
                lower: RampEnd {
                    position: lower.position,
                    endpoints: lower.endpoints,
                    width_pixels: fixed_to_pixels(lower.width_fixed),
                },
                upper: RampEnd {
                    position: upper.position,
                    endpoints: upper.endpoints,
                    width_pixels: fixed_to_pixels(upper.width_fixed),
                },
                cell_count: u32::try_from(component.len()).expect("grid cell count is bounded"),
            });
        }
        ramps
    }

    fn is_walkable_ramp_index(&self, index: usize) -> bool {
        self.cells
            .get(index)
            .is_some_and(|cell| cell.walkable && cell.ramp)
    }

    fn ramp_component(&self, start: WalkPosition, visited: &mut [bool]) -> Vec<WalkPosition> {
        let mut component = vec![start];
        let Some(start_index) = self.index(start) else {
            return Vec::new();
        };
        visited[start_index] = true;
        let mut cursor = 0;
        while let Some(&position) = component.get(cursor) {
            cursor += 1;
            for (neighbor, _) in self.neighbors(position) {
                let Some(index) = self.index(neighbor) else {
                    continue;
                };
                if !visited[index] && self.is_walkable_ramp_index(index) {
                    visited[index] = true;
                    component.push(neighbor);
                }
            }
        }
        component
    }

    fn add_elevation_sample(
        &self,
        position: WalkPosition,
        epoch: u32,
        marks: &mut [u32],
        counts: &mut [u64; 3],
        sums: &mut [(i64, i64); 3],
    ) {
        let Some(index) = self.index(position) else {
            return;
        };
        if marks[index] == epoch {
            return;
        }
        let Some(cell) = self.cell(position) else {
            return;
        };
        if !cell.walkable {
            return;
        }
        let elevation = usize::from(cell.elevation);
        if elevation >= counts.len() {
            return;
        }
        marks[index] = epoch;
        counts[elevation] += 1;
        sums[elevation].0 += i64::from(position.x);
        sums[elevation].1 += i64::from(position.y);
    }

    fn has_adjacent_elevation_transition(&self, component: &[WalkPosition]) -> bool {
        component.iter().copied().any(|position| {
            let Some(cell) = self.cell(position) else {
                return false;
            };
            self.neighbors(position).any(|(neighbor, _)| {
                self.cell(neighbor)
                    .is_some_and(|other| cell.elevation.abs_diff(other.elevation) == 1)
            })
        })
    }

    fn initial_shoulder(
        &self,
        component: &[WalkPosition],
        search: EndSearch,
        original: EndCandidate,
    ) -> EndCandidate {
        let Some(count) = i64::try_from(component.len()).ok() else {
            return original;
        };
        let component_extent = component
            .iter()
            .copied()
            .map(|position| {
                let longitudinal = project(position, search.axis);
                if search.lower {
                    longitudinal.saturating_sub(search.extreme)
                } else {
                    search.extreme.saturating_sub(longitudinal)
                }
            })
            .max()
            .unwrap_or(0);
        let maximum_distance = INITIAL_SHOULDER_DISTANCE
            .min(component_extent.saturating_sub(MIN_END_SEPARATION_CELLS) / 2);
        for end_distance in (END_BAND_CELLS + 1)..=maximum_distance {
            let candidate = component
                .iter()
                .copied()
                .filter_map(|position| {
                    let longitudinal = project(position, search.axis);
                    let distance = if search.lower {
                        longitudinal.saturating_sub(search.extreme)
                    } else {
                        search.extreme.saturating_sub(longitudinal)
                    };
                    (distance == end_distance).then_some((position, longitudinal))
                })
                .filter_map(|(position, longitudinal)| {
                    let (width_fixed, endpoints) =
                        self.bounded_wall_span(position, search.normal)?;
                    Some((
                        EndCandidate {
                            position,
                            longitudinal,
                            width_fixed,
                            endpoints,
                        },
                        (project(position, search.normal)
                            .saturating_mul(count)
                            .saturating_sub(search.transverse_sum))
                        .abs(),
                    ))
                })
                .min_by_key(|(candidate, lateral_distance)| {
                    let endpoints = canonical_endpoints(candidate.endpoints);
                    (
                        *lateral_distance,
                        candidate.width_fixed,
                        endpoints[0].y,
                        endpoints[0].x,
                        endpoints[1].y,
                        endpoints[1].x,
                        candidate.position.y,
                        candidate.position.x,
                    )
                })
                .map(|(candidate, _)| candidate);
            let Some(candidate) = candidate else {
                continue;
            };
            if candidate
                .width_fixed
                .saturating_add(MIN_OPENING_INCREASE_FIXED)
                <= original.width_fixed
                && candidate.width_fixed.saturating_mul(100)
                    <= original.width_fixed.saturating_mul(75)
            {
                return candidate;
            }
        }
        original
    }
    fn refine_end(
        &self,
        original: EndCandidate,
        axis: (i32, i32),
        normal: (i32, i32),
        direction: (i32, i32),
    ) -> EndCandidate {
        let Some(origin) = self.cell(original.position) else {
            return original;
        };
        if origin.terrain_buildable {
            return original;
        }
        let elevation = origin.elevation;
        let mut position = original.position;
        let mut last_narrow = original;
        let mut opening = None;
        let mut reached_landing = false;
        let mut widened = false;
        for _ in 0..MAX_LANDING_STEPS {
            let Some(x) = position.x.checked_add_signed(direction.0) else {
                break;
            };
            let Some(y) = position.y.checked_add_signed(direction.1) else {
                break;
            };
            let next_position = WalkPosition { x, y };
            if !self
                .neighbors(position)
                .any(|(neighbor, _)| neighbor == next_position)
            {
                break;
            }
            let Some(next_cell) = self.cell(next_position) else {
                break;
            };
            if next_cell.terrain_buildable {
                reached_landing = next_cell.elevation == elevation;
                break;
            }
            if next_cell.elevation != elevation {
                break;
            }
            position = next_position;
            let Some((width_fixed, endpoints)) = self.bounded_wall_span(next_position, normal)
            else {
                widened = true;
                continue;
            };
            let current = EndCandidate {
                position: next_position,
                longitudinal: project(next_position, axis),
                width_fixed,
                endpoints,
            };
            if widened {
                continue;
            }
            if is_widened(current.width_fixed, original.width_fixed) {
                widened = true;
                if is_local_opening(current.width_fixed, original.width_fixed) {
                    opening = Some(current);
                }
            } else {
                last_narrow = current;
            }
        }
        if reached_landing {
            opening.unwrap_or(last_narrow)
        } else {
            original
        }
    }

    fn select_end(
        &self,
        component: &[WalkPosition],
        axis: (i32, i32),
        normal: (i32, i32),
        extreme: i64,
        transverse_sum: i64,
        lower: bool,
    ) -> Option<EndCandidate> {
        let count = i64::try_from(component.len()).ok()?;
        component
            .iter()
            .copied()
            .filter_map(|position| {
                let longitudinal = project(position, axis);
                let end_distance = if lower {
                    longitudinal.saturating_sub(extreme)
                } else {
                    extreme.saturating_sub(longitudinal)
                };
                (end_distance <= END_BAND_CELLS).then_some((position, longitudinal, end_distance))
            })
            .filter_map(|(position, longitudinal, end_distance)| {
                let (width_fixed, endpoints) = self.bounded_wall_span(position, normal)?;
                Some((
                    EndCandidate {
                        position,
                        longitudinal,
                        width_fixed,
                        endpoints,
                    },
                    (project(position, normal)
                        .saturating_mul(count)
                        .saturating_sub(transverse_sum))
                    .abs(),
                    end_distance,
                ))
            })
            .min_by_key(|(candidate, lateral_distance, end_distance)| {
                let endpoints = canonical_endpoints(candidate.endpoints);
                (
                    *lateral_distance,
                    *end_distance,
                    candidate.width_fixed,
                    endpoints[0].y,
                    endpoints[0].x,
                    endpoints[1].y,
                    endpoints[1].x,
                    candidate.position.y,
                    candidate.position.x,
                )
            })
            .map(|(candidate, _, _)| candidate)
    }
}

fn is_widened(width_fixed: u64, baseline_fixed: u64) -> bool {
    width_fixed >= baseline_fixed.saturating_add(MIN_OPENING_INCREASE_FIXED)
        && width_fixed.saturating_mul(100) >= baseline_fixed.saturating_mul(175)
}

fn is_local_opening(width_fixed: u64, baseline_fixed: u64) -> bool {
    width_fixed <= MAX_LOCAL_OPENING_FIXED && width_fixed <= baseline_fixed.saturating_mul(4)
}

fn project(position: WalkPosition, direction: (i32, i32)) -> i64 {
    i64::from(position.x) * i64::from(direction.0) + i64::from(position.y) * i64::from(direction.1)
}

fn quantized_centroid_direction(
    lower_sum: (i64, i64),
    lower_count: u64,
    upper_sum: (i64, i64),
    upper_count: u64,
) -> Option<(i32, i32)> {
    let lower_count = i128::from(lower_count);
    let upper_count = i128::from(upper_count);
    let dx = i128::from(upper_sum.0) * lower_count - i128::from(lower_sum.0) * upper_count;
    let dy = i128::from(upper_sum.1) * lower_count - i128::from(lower_sum.1) * upper_count;
    if dx == 0 && dy == 0 {
        return None;
    }
    let sx = if dx < 0 { -1 } else { 1 };
    let sy = if dy < 0 { -1 } else { 1 };
    let absolute_x = dx.unsigned_abs();
    let absolute_y = dy.unsigned_abs();
    if absolute_y.saturating_mul(2) < absolute_x {
        Some((sx, 0))
    } else if absolute_x.saturating_mul(2) < absolute_y {
        Some((0, sy))
    } else {
        Some((sx, sy))
    }
}

fn canonical_endpoints(endpoints: [PixelPosition; 2]) -> [PixelPosition; 2] {
    if (endpoints[0].y, endpoints[0].x) <= (endpoints[1].y, endpoints[1].x) {
        endpoints
    } else {
        [endpoints[1], endpoints[0]]
    }
}

fn fixed_to_pixels(width_fixed: u64) -> f64 {
    width_fixed as f64 * PIXELS_PER_WALK_CELL / FIXED_PER_WALK_CELL
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerrainCell;

    const SIZE: u32 = 33;

    fn point(x: u32, y: u32) -> WalkPosition {
        WalkPosition { x, y }
    }

    fn grid_for_axis(axis: (i32, i32)) -> TerrainGrid {
        let center = (16_i32, 16_i32);
        let normal = (-axis.1, axis.0);
        let mut cells = Vec::new();
        for y in 0..SIZE {
            for x in 0..SIZE {
                let dx = i32::try_from(x).unwrap() - center.0;
                let dy = i32::try_from(y).unwrap() - center.1;
                let longitudinal = dx * axis.0 + dy * axis.1;
                let transverse = dx * normal.0 + dy * normal.1;
                let walkable = (-14..=14).contains(&longitudinal) && (-8..=8).contains(&transverse);
                let ramp =
                    walkable && (-5..=5).contains(&longitudinal) && (-3..=3).contains(&transverse);
                cells.push(TerrainCell {
                    walkable,
                    terrain_buildable: true,
                    elevation: if ramp && longitudinal >= 0 { 1 } else { 0 },
                    ramp,
                });
            }
        }
        TerrainGrid::from_cells(SIZE, SIZE, cells).unwrap()
    }

    fn ramp_with_end_apron(axis: (i32, i32)) -> TerrainGrid {
        const APRON_SIZE: u32 = 65;
        let center = 32_i32;
        let normal = (-axis.1, axis.0);
        let mut cells = Vec::new();
        for y in 0..APRON_SIZE {
            for x in 0..APRON_SIZE {
                let dx = i32::try_from(x).unwrap() - center;
                let dy = i32::try_from(y).unwrap() - center;
                let longitudinal = dx * axis.0 + dy * axis.1;
                let transverse = dx * normal.0 + dy * normal.1;
                let corridor = (-10..=10).contains(&longitudinal) && (-8..=8).contains(&transverse);
                let apron = (-5..=-3).contains(&longitudinal) && (-24..=24).contains(&transverse);
                let walkable = corridor || apron;
                let ramp =
                    walkable && (-5..=5).contains(&longitudinal) && (-3..=3).contains(&transverse);
                cells.push(TerrainCell {
                    walkable,
                    terrain_buildable: true,
                    elevation: if longitudinal >= 0 { 1 } else { 0 },
                    ramp,
                });
            }
        }
        TerrainGrid::from_cells(APRON_SIZE, APRON_SIZE, cells).unwrap()
    }

    fn short_double_apron_grid() -> TerrainGrid {
        const SHORT_SIZE: u32 = 65;
        let center = 32_i32;
        let mut cells = Vec::new();
        for y in 0..SHORT_SIZE {
            for x in 0..SHORT_SIZE {
                let longitudinal = i32::try_from(x).unwrap() - center;
                let transverse = i32::try_from(y).unwrap() - center;
                let corridor = (-3..=3).contains(&longitudinal) && (-8..=8).contains(&transverse);
                let apron = ((-3..=-1).contains(&longitudinal) || (1..=3).contains(&longitudinal))
                    && (-24..=24).contains(&transverse);
                let walkable = corridor || apron;
                let ramp =
                    walkable && (-3..=3).contains(&longitudinal) && (-3..=3).contains(&transverse);
                cells.push(TerrainCell {
                    walkable,
                    terrain_buildable: true,
                    elevation: if longitudinal >= 0 { 1 } else { 0 },
                    ramp,
                });
            }
        }
        TerrainGrid::from_cells(SHORT_SIZE, SHORT_SIZE, cells).unwrap()
    }
    fn naturally_wide_ramp_grid(axis: (i32, i32)) -> TerrainGrid {
        const WIDE_SIZE: u32 = 65;
        let center = 32_i32;
        let normal = (-axis.1, axis.0);
        let mut cells = Vec::new();
        for y in 0..WIDE_SIZE {
            for x in 0..WIDE_SIZE {
                let dx = i32::try_from(x).unwrap() - center;
                let dy = i32::try_from(y).unwrap() - center;
                let longitudinal = dx * axis.0 + dy * axis.1;
                let transverse = dx * normal.0 + dy * normal.1;
                let walkable =
                    (-10..=10).contains(&longitudinal) && (-24..=24).contains(&transverse);
                let ramp =
                    walkable && (-5..=5).contains(&longitudinal) && (-3..=3).contains(&transverse);
                cells.push(TerrainCell {
                    walkable,
                    terrain_buildable: true,
                    elevation: if longitudinal >= 0 { 1 } else { 0 },
                    ramp,
                });
            }
        }
        TerrainGrid::from_cells(WIDE_SIZE, WIDE_SIZE, cells).unwrap()
    }
    fn unbuildable_slope_grid(axis: (i32, i32), landing: bool) -> TerrainGrid {
        let mut grid = grid_for_axis(axis);
        let center = (16_i32, 16_i32);
        for y in 0..SIZE {
            for x in 0..SIZE {
                let dx = i32::try_from(x).unwrap() - center.0;
                let dy = i32::try_from(y).unwrap() - center.1;
                let longitudinal = dx * axis.0 + dy * axis.1;
                let cell = grid.cell(point(x, y)).unwrap();
                if cell.walkable {
                    let index = usize::try_from(y * SIZE + x).unwrap();
                    grid.cells[index].elevation = if longitudinal >= 0 { 1 } else { 0 };
                    if !landing || (-8..=8).contains(&longitudinal) {
                        grid.cells[index].terrain_buildable = false;
                    }
                }
            }
        }
        grid
    }

    fn long_slope_grid(unbounded_gap: bool) -> TerrainGrid {
        const LARGE_SIZE: u32 = 201;
        let center = 100_i32;
        let mut cells = Vec::new();
        for y in 0..LARGE_SIZE {
            for x in 0..LARGE_SIZE {
                let longitudinal = i32::try_from(x).unwrap() - center;
                let transverse = i32::try_from(y).unwrap() - center;
                let in_corridor =
                    (-14..=14).contains(&longitudinal) && (-8..=8).contains(&transverse);
                let open_gap = unbounded_gap && longitudinal == -6;
                let walkable = in_corridor || open_gap;
                let ramp =
                    walkable && (-5..=5).contains(&longitudinal) && (-3..=3).contains(&transverse);
                cells.push(TerrainCell {
                    walkable,
                    terrain_buildable: !(-8..=8).contains(&longitudinal),
                    elevation: if longitudinal >= 0 { 1 } else { 0 },
                    ramp,
                });
            }
        }
        TerrainGrid::from_cells(LARGE_SIZE, LARGE_SIZE, cells).unwrap()
    }
    fn opening_grid() -> TerrainGrid {
        let axis = (1, 0);
        let center = (16_i32, 16_i32);
        let mut grid = unbuildable_slope_grid(axis, true);
        for y in 0..SIZE {
            for x in 0..SIZE {
                let longitudinal = i32::try_from(x).unwrap() - center.0;
                let transverse = i32::try_from(y).unwrap() - center.1;
                if (-14..=14).contains(&longitudinal)
                    && longitudinal.unsigned_abs() >= 6
                    && transverse.unsigned_abs() <= 15
                {
                    let index = usize::try_from(y * SIZE + x).unwrap();
                    grid.cells[index].walkable = true;
                    if (-8..=8).contains(&longitudinal) {
                        grid.cells[index].terrain_buildable = false;
                    }
                }
            }
        }
        grid
    }

    fn inferred_axis(ramp: &Ramp) -> (i32, i32) {
        quantize(
            i64::from(ramp.upper.position.x) - i64::from(ramp.lower.position.x),
            i64::from(ramp.upper.position.y) - i64::from(ramp.lower.position.y),
        )
        .unwrap()
    }

    fn quantize(dx: i64, dy: i64) -> Option<(i32, i32)> {
        if dx == 0 && dy == 0 {
            return None;
        }
        let sx = if dx < 0 { -1 } else { 1 };
        let sy = if dy < 0 { -1 } else { 1 };
        if dy.unsigned_abs().saturating_mul(2) < dx.unsigned_abs() {
            Some((sx, 0))
        } else if dx.unsigned_abs().saturating_mul(2) < dy.unsigned_abs() {
            Some((0, sy))
        } else {
            Some((sx, sy))
        }
    }

    #[test]
    fn cardinal_and_diagonal_rotations_emit_two_distinct_finite_ends() {
        for axis in [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ] {
            let grid = grid_for_axis(axis);
            let ramps = grid.ramps();
            assert_eq!(ramps.len(), 1, "axis={axis:?}");
            let ramp = &ramps[0];
            assert_eq!(ramp.id, 1);
            assert_eq!((ramp.lower_elevation, ramp.upper_elevation), (0, 1));
            assert_ne!(ramp.lower.position, ramp.upper.position);
            assert_ne!(
                canonical_endpoints(ramp.lower.endpoints),
                canonical_endpoints(ramp.upper.endpoints)
            );
            assert_eq!(inferred_axis(ramp), axis, "axis={axis:?}");
            let normal = (-axis.1, axis.0);
            for end in [&ramp.lower, &ramp.upper] {
                let (width_fixed, endpoints) =
                    grid.bounded_wall_span(end.position, normal).unwrap();
                assert_eq!(end.endpoints, endpoints);
                assert!((end.width_pixels - fixed_to_pixels(width_fixed)).abs() < f64::EPSILON);
            }
        }
    }

    #[test]
    fn initial_shoulder_replaces_an_abrupt_end_apron_in_all_orientations() {
        for axis in [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ] {
            let grid = ramp_with_end_apron(axis);
            let ramp = grid.ramps().pop().unwrap();
            let center_projection = 32 * i64::from(axis.0 + axis.1);
            assert_eq!(
                project(ramp.lower.position, axis),
                center_projection - 2,
                "axis={axis:?}"
            );
            assert!(ramp.lower.width_pixels < 200.0, "axis={axis:?}");
            assert!(ramp.upper.width_pixels < 200.0, "axis={axis:?}");
        }
    }

    #[test]
    fn short_double_apron_keeps_two_separated_outer_ends() {
        let ramp = short_double_apron_grid().ramps().pop().unwrap();
        assert!(ramp.lower.width_pixels >= 300.0);
        assert!(ramp.upper.width_pixels >= 300.0);
        assert!(ramp.upper.position.x - ramp.lower.position.x >= 4);
    }
    #[test]
    fn naturally_wide_ramps_keep_their_outer_end_band() {
        for axis in [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ] {
            let grid = naturally_wide_ramp_grid(axis);
            let ramp = grid.ramps().pop().unwrap();
            let center_projection = 32 * i64::from(axis.0 + axis.1);
            assert!(ramp.lower.width_pixels >= 250.0, "axis={axis:?}");
            assert!(ramp.upper.width_pixels >= 250.0, "axis={axis:?}");
            assert!(
                project(ramp.lower.position, axis) <= center_projection - 3,
                "axis={axis:?}"
            );
            assert!(
                project(ramp.upper.position, axis) >= center_projection + 3,
                "axis={axis:?}"
            );
        }
    }
    #[test]
    fn unbuildable_slope_extensions_work_for_all_rotations_and_reflections() {
        for axis in [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ] {
            let grid = unbuildable_slope_grid(axis, true);
            let ramp = grid.ramps().pop().unwrap();
            assert!(
                !grid.cell(ramp.lower.position).unwrap().ramp,
                "axis={axis:?} lower={:?} upper={:?}",
                ramp.lower.position,
                ramp.upper.position
            );
            assert!(
                !grid.cell(ramp.upper.position).unwrap().ramp,
                "axis={axis:?} lower={:?} upper={:?}",
                ramp.lower.position,
                ramp.upper.position
            );
            assert!(!grid.cell(ramp.lower.position).unwrap().terrain_buildable);
            assert!(!grid.cell(ramp.upper.position).unwrap().terrain_buildable);
            assert_eq!(inferred_axis(&ramp), axis, "axis={axis:?}");
        }
    }

    #[test]
    fn first_bounded_local_opening_is_used_before_the_buildable_landing() {
        let baseline = grid_for_axis((1, 0)).ramps().pop().unwrap();
        let ramp = opening_grid().ramps().pop().unwrap();
        assert!(!opening_grid().cell(ramp.lower.position).unwrap().ramp);
        assert!(!opening_grid().cell(ramp.upper.position).unwrap().ramp);
        assert!(ramp.lower.width_pixels > baseline.lower.width_pixels);
        assert!(ramp.upper.width_pixels > baseline.upper.width_pixels);
        assert!(ramp.lower.width_pixels <= 384.0);
        assert!(ramp.upper.width_pixels <= 384.0);
    }

    #[test]
    fn unbounded_section_does_not_select_a_later_finite_chord() {
        let gap = long_slope_grid(true);
        let ramp = gap.ramps().pop().unwrap();
        assert_eq!(ramp.lower.position, point(95, 100));
        assert!(gap.bounded_wall_span(point(93, 100), (0, 1)).is_some());
        assert!(!gap.cell(ramp.upper.position).unwrap().ramp);
    }
    #[test]
    fn missing_landing_keeps_the_flagged_component_ends() {
        let baseline = grid_for_axis((1, 0)).ramps().pop().unwrap();
        let ramp = unbuildable_slope_grid((1, 0), false).ramps().pop().unwrap();
        assert_eq!(ramp.lower.position, baseline.lower.position);
        assert_eq!(ramp.upper.position, baseline.upper.position);
        assert!(ramp.lower.width_pixels <= 384.0);
        assert!(ramp.upper.width_pixels <= 384.0);
    }

    #[test]
    fn flat_flags_ambiguous_elevations_and_isolated_bits_do_not_emit_ramps() {
        let flat = TerrainGrid::from_cells(
            8,
            8,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                64
            ],
        )
        .unwrap();
        assert!(flat.ramps().is_empty());

        let mut ambiguous = grid_for_axis((1, 0));
        let index = usize::try_from(16_u32 * SIZE + 16).unwrap();
        ambiguous.cells[index].elevation = 2;
        assert!(ambiguous.ramps().is_empty());

        let mut isolated = flat.clone();
        let index = usize::try_from(4_u32 * 8 + 4).unwrap();
        isolated.cells[index].ramp = true;
        isolated.cells[index].elevation = 1;
        assert!(isolated.ramps().is_empty());
    }

    #[test]
    fn map_edge_and_unbounded_sections_do_not_emit_ramps() {
        let mut cells = vec![
            TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            32 * 32
        ];
        for y in 12..=18 {
            for x in 10..=21 {
                let index = y * 32 + x;
                cells[index].ramp = true;
                cells[index].elevation = if x < 16 { 0 } else { 1 };
            }
        }
        let open = TerrainGrid::from_cells(32, 32, cells).unwrap();
        assert!(open.ramps().is_empty());
    }

    #[test]
    fn row_major_emitted_ids_are_deterministic() {
        let first = grid_for_axis((1, 0));
        let second = grid_for_axis((1, 0));
        let mut cells = vec![
            TerrainCell {
                walkable: false,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            70 * 70
        ];
        for (source, offset_y) in [(&first, 2_u32), (&second, 35_u32)] {
            for y in 0..SIZE {
                for x in 0..SIZE {
                    let source_cell = source.cell(point(x, y)).unwrap();
                    let target = usize::try_from((y + offset_y) * 70 + (x + 2)).unwrap();
                    cells[target] = *source_cell;
                }
            }
        }
        let grid = TerrainGrid::from_cells(70, 70, cells).unwrap();
        let ramps = grid.ramps();
        assert_eq!(ramps.iter().map(|ramp| ramp.id).collect::<Vec<_>>(), [1, 2]);
        assert!(ramps[0].lower.position.y < ramps[1].lower.position.y);
    }

    #[test]
    fn maximum_grid_without_ramps_is_bounded() {
        let grid = TerrainGrid::from_cells(
            1024,
            1024,
            vec![
                TerrainCell {
                    walkable: false,
                    terrain_buildable: false,
                    elevation: 0,
                    ramp: false,
                };
                1024 * 1024
            ],
        )
        .unwrap();
        assert!(grid.ramps().is_empty());
    }
}
