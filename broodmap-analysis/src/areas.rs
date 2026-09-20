//! Exploratory graph partitions induced by finite pixel-space boundary spans.
//!
//! A [`BoundarySpan`] removes only existing walk-cell graph edges that cross its finite line
//! segment. It is evidence about a possible boundary, rather than a claim that the span is a
//! wall that units cannot cross in the game.

use std::collections::VecDeque;

use thiserror::Error;

use crate::{PixelPosition, TerrainGrid, WalkPosition};

const MAX_BOUNDARY_SPANS: usize = 256;
const PIXELS_PER_WALK_CELL: u32 = 8;

/// A finite boundary segment in logical map pixels.
///
/// Endpoint coordinates are inclusive: an endpoint on the map's right or bottom edge is valid.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BoundarySpan {
    pub endpoints: [PixelPosition; 2],
}

/// An error while partitioning walkable terrain by boundary spans.
#[derive(Debug, Error, Copy, Clone, Eq, PartialEq)]
pub enum AreaError {
    #[error("at most {MAX_BOUNDARY_SPANS} boundary spans are supported, got {actual}")]
    TooManyBoundarySpans { actual: usize },
    #[error(
        "boundary span {span} endpoint {endpoint} ({x}, {y}) is outside the inclusive {width_pixels}x{height_pixels} pixel map bounds"
    )]
    EndpointOutOfBounds {
        span: usize,
        endpoint: usize,
        x: u32,
        y: u32,
        width_pixels: u32,
        height_pixels: u32,
    },
    #[error("boundary span {span} has coincident endpoints ({x}, {y})")]
    ZeroLengthSpan { span: usize, x: u32, y: u32 },
}

/// One contiguous walkable component in a [`SpanPartition`].
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PartitionArea {
    /// Contiguous one-based ID, assigned in row-major component order.
    pub id: u32,
    /// Number of original walkable cells in this component.
    pub cell_count: u32,
}

/// The effect of one input boundary span after all supplied spans have been applied.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BoundaryAssessment {
    /// Number of original legal graph edges this span removes.
    pub removed_edge_count: u32,
    /// Number of those removed edges whose endpoints are in different final components.
    pub separated_edge_count: u32,
    /// Sorted, unique final component pairs observed across this span's removed edges.
    pub region_pairs: Vec<[u32; 2]>,
    /// Sorted, unique final component IDs touched by either endpoint of every removed edge.
    /// Unlike `region_pairs`, this retains the area for bypassable edges whose labels match.
    pub incident_area_ids: Vec<u32>,
}

/// Immutable labels and boundary evidence from [`TerrainGrid::partition_by_spans`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SpanPartition {
    width: u32,
    height: u32,
    labels: Vec<u32>,
    areas: Vec<PartitionArea>,
    boundaries: Vec<BoundaryAssessment>,
}

impl SpanPartition {
    /// Width in 8-pixel walk cells.
    pub fn width(&self) -> u32 {
        self.width
    }

    /// Height in 8-pixel walk cells.
    pub fn height(&self) -> u32 {
        self.height
    }

    /// Row-major component labels. Terrain-blocked cells are zero.
    pub fn labels(&self) -> &[u32] {
        &self.labels
    }

    /// Contiguous components in ID order.
    pub fn areas(&self) -> &[PartitionArea] {
        &self.areas
    }

    /// Boundary assessments in the original input order.
    pub fn boundaries(&self) -> &[BoundaryAssessment] {
        &self.boundaries
    }
}

impl TerrainGrid {
    /// Partitions the original walkable graph after removing edges crossed by finite boundary spans.
    ///
    /// Walk cells remain vertices; this method neither changes the grid nor alters routing. A
    /// vertex uses its center `(x * 8 + 4, y * 8 + 4)` in logical pixels. For a vertex exactly on
    /// a span's supporting line, its canonical signed side is non-negative. Thus a line through a
    /// row of centers does not automatically disconnect that entire row from both sides.
    pub fn partition_by_spans(&self, spans: &[BoundarySpan]) -> Result<SpanPartition, AreaError> {
        validate_spans(self, spans)?;

        // Each bit marks a removed outgoing neighbor edge. Candidate cells are a conservative
        // constant-width strip around each finite span, then `cuts` remains the exact decision.
        let mut cut_masks = vec![0_u16; self.cells.len()];
        let mut span_edges = Vec::with_capacity(spans.len());
        for span in spans {
            let geometry = SpanGeometry::new(*span);
            let mut edges = Vec::new();
            for position in candidate_positions(geometry, self.width, self.height) {
                let index = self
                    .index(position)
                    .expect("span candidate must remain in bounds");
                if !self.cells[index].walkable {
                    continue;
                }
                for (neighbor, _) in self.neighbors(position) {
                    let neighbor_index = self
                        .index(neighbor)
                        .expect("TerrainGrid neighbor must remain in bounds");
                    if index >= neighbor_index || !geometry.cuts(position, neighbor) {
                        continue;
                    }
                    cut_masks[index] |= edge_bit(position, neighbor);
                    cut_masks[neighbor_index] |= edge_bit(neighbor, position);
                    edges.push((index, neighbor_index));
                }
            }
            span_edges.push(edges);
        }

        let (labels, areas) = components(self, &cut_masks);
        let boundaries = span_edges
            .iter()
            .map(|edges| boundary_assessment(edges, &labels))
            .collect();
        Ok(SpanPartition {
            width: self.width,
            height: self.height,
            labels,
            areas,
            boundaries,
        })
    }
}

fn validate_spans(grid: &TerrainGrid, spans: &[BoundarySpan]) -> Result<(), AreaError> {
    if spans.len() > MAX_BOUNDARY_SPANS {
        return Err(AreaError::TooManyBoundarySpans {
            actual: spans.len(),
        });
    }
    let width_pixels = grid.width * PIXELS_PER_WALK_CELL;
    let height_pixels = grid.height * PIXELS_PER_WALK_CELL;
    for (span_index, span) in spans.iter().enumerate() {
        for (endpoint_index, endpoint) in span.endpoints.iter().enumerate() {
            if endpoint.x > width_pixels || endpoint.y > height_pixels {
                return Err(AreaError::EndpointOutOfBounds {
                    span: span_index,
                    endpoint: endpoint_index,
                    x: endpoint.x,
                    y: endpoint.y,
                    width_pixels,
                    height_pixels,
                });
            }
        }
        if span.endpoints[0] == span.endpoints[1] {
            return Err(AreaError::ZeroLengthSpan {
                span: span_index,
                x: span.endpoints[0].x,
                y: span.endpoints[0].y,
            });
        }
    }
    Ok(())
}

fn components(grid: &TerrainGrid, cut_masks: &[u16]) -> (Vec<u32>, Vec<PartitionArea>) {
    let mut labels = vec![0_u32; grid.cells.len()];
    let mut areas = Vec::new();
    let mut queue = VecDeque::new();
    for start in 0..grid.cells.len() {
        if !grid.cells[start].walkable || labels[start] != 0 {
            continue;
        }
        let id =
            u32::try_from(areas.len() + 1).expect("validated grid has at most one million cells");
        let mut cell_count = 0_u32;
        labels[start] = id;
        queue.push_back(start);
        while let Some(index) = queue.pop_front() {
            cell_count += 1;
            let position = grid.position(index);
            for (neighbor, _) in grid.neighbors(position) {
                let neighbor_index = grid
                    .index(neighbor)
                    .expect("TerrainGrid neighbor must remain in bounds");
                if cut_masks[index] & edge_bit(position, neighbor) != 0
                    || labels[neighbor_index] != 0
                {
                    continue;
                }
                labels[neighbor_index] = id;
                queue.push_back(neighbor_index);
            }
        }
        areas.push(PartitionArea { id, cell_count });
    }
    (labels, areas)
}

fn boundary_assessment(edges: &[(usize, usize)], labels: &[u32]) -> BoundaryAssessment {
    let mut incident_area_ids = Vec::with_capacity(edges.len() * 2);
    let mut region_pairs = Vec::new();
    let mut separated_edge_count = 0_u32;
    for &(left, right) in edges {
        let left_label = labels[left];
        let right_label = labels[right];
        incident_area_ids.extend([left_label, right_label]);
        if left_label == right_label {
            continue;
        }
        separated_edge_count += 1;
        region_pairs.push([left_label.min(right_label), left_label.max(right_label)]);
    }
    incident_area_ids.sort_unstable();
    incident_area_ids.dedup();
    region_pairs.sort_unstable();
    region_pairs.dedup();
    BoundaryAssessment {
        removed_edge_count: u32::try_from(edges.len())
            .expect("validated grid has at most four million graph edges"),
        separated_edge_count,
        region_pairs,
        incident_area_ids,
    }
}

fn edge_bit(from: WalkPosition, to: WalkPosition) -> u16 {
    let delta_x = to.x as i64 - from.x as i64;
    let delta_y = to.y as i64 - from.y as i64;
    debug_assert!((-1..=1).contains(&delta_x) && (-1..=1).contains(&delta_y));
    debug_assert!(delta_x != 0 || delta_y != 0);
    1_u16 << ((delta_y + 1) * 3 + (delta_x + 1))
}

const CANDIDATE_MARGIN_PIXELS: i64 = PIXELS_PER_WALK_CELL as i64 * 2;
const CANDIDATE_MINOR_RADIUS_CELLS: i64 = 3;

/// Enumerates a constant-width supercover strip around a finite span.
///
/// If a one-cell graph edge intersects the span, either endpoint center is at most eight pixels
/// from that intersection in each axis. Advancing to the center-coordinate column/row used for
/// the strip can move the line by at most another eight pixels in its minor axis. The three-cell
/// minor radius therefore contains every possible edge endpoint, including closed endpoints and
/// diagonal edges. `SpanGeometry::cuts` remains the exact predicate for each candidate edge.
fn candidate_positions(geometry: SpanGeometry, width: u32, height: u32) -> Vec<WalkPosition> {
    let delta_x = i64::from(geometry.b.x) - i64::from(geometry.a.x);
    let delta_y = i64::from(geometry.b.y) - i64::from(geometry.a.y);
    if delta_x.abs() >= delta_y.abs() {
        let mut positions = Vec::new();
        for x in candidate_axis_range(geometry.min_x, geometry.max_x, width) {
            let center_x = i64::from(x) * i64::from(PIXELS_PER_WALK_CELL)
                + i64::from(PIXELS_PER_WALK_CELL / 2);
            let line_x = center_x.clamp(i64::from(geometry.a.x), i64::from(geometry.b.x));
            let line_y = (i64::from(geometry.a.y) * delta_x
                + delta_y * (line_x - i64::from(geometry.a.x)))
            .div_euclid(delta_x);
            for y in candidate_minor_range(line_y, height) {
                positions.push(WalkPosition { x, y });
            }
        }
        positions
    } else {
        let (lower, upper) = if geometry.a.y <= geometry.b.y {
            (geometry.a, geometry.b)
        } else {
            (geometry.b, geometry.a)
        };
        let rise = i64::from(upper.y) - i64::from(lower.y);
        let run = i64::from(upper.x) - i64::from(lower.x);
        let mut positions = Vec::new();
        for y in candidate_axis_range(geometry.min_y, geometry.max_y, height) {
            let center_y = i64::from(y) * i64::from(PIXELS_PER_WALK_CELL)
                + i64::from(PIXELS_PER_WALK_CELL / 2);
            let line_y = center_y.clamp(i64::from(lower.y), i64::from(upper.y));
            let line_x =
                (i64::from(lower.x) * rise + run * (line_y - i64::from(lower.y))).div_euclid(rise);
            for x in candidate_minor_range(line_x, width) {
                positions.push(WalkPosition { x, y });
            }
        }
        positions
    }
}

fn candidate_axis_range(minimum: u32, maximum: u32, limit: u32) -> std::ops::RangeInclusive<u32> {
    let first =
        (i64::from(minimum) - CANDIDATE_MARGIN_PIXELS).max(0) / i64::from(PIXELS_PER_WALK_CELL);
    let last = ((i64::from(maximum) + CANDIDATE_MARGIN_PIXELS) / i64::from(PIXELS_PER_WALK_CELL))
        .min(i64::from(limit - 1));
    u32::try_from(first).expect("candidate range stays nonnegative")
        ..=u32::try_from(last).expect("candidate range stays within the grid")
}

fn candidate_minor_range(reference_pixels: i64, limit: u32) -> std::ops::RangeInclusive<u32> {
    let center = reference_pixels.div_euclid(i64::from(PIXELS_PER_WALK_CELL));
    let first = (center - CANDIDATE_MINOR_RADIUS_CELLS).max(0);
    let last = (center + CANDIDATE_MINOR_RADIUS_CELLS).min(i64::from(limit - 1));
    u32::try_from(first).expect("candidate range stays nonnegative")
        ..=u32::try_from(last).expect("candidate range stays within the grid")
}

#[derive(Debug, Copy, Clone)]
struct SpanGeometry {
    a: PixelPosition,
    b: PixelPosition,
    min_x: u32,
    max_x: u32,
    min_y: u32,
    max_y: u32,
}

impl SpanGeometry {
    fn new(span: BoundarySpan) -> Self {
        let [first, second] = span.endpoints;
        let (a, b) = if first <= second {
            (first, second)
        } else {
            (second, first)
        };
        Self {
            a,
            b,
            min_x: a.x.min(b.x),
            max_x: a.x.max(b.x),
            min_y: a.y.min(b.y),
            max_y: a.y.max(b.y),
        }
    }

    fn cuts(self, from: WalkPosition, to: WalkPosition) -> bool {
        let from = cell_center(from);
        let to = cell_center(to);
        let from_side = cross(self.a, self.b, from);
        let to_side = cross(self.a, self.b, to);
        (from_side >= 0) != (to_side >= 0) && segments_intersect(self.a, self.b, from, to)
    }
}

fn cell_center(position: WalkPosition) -> PixelPosition {
    PixelPosition {
        x: position.x * PIXELS_PER_WALK_CELL + PIXELS_PER_WALK_CELL / 2,
        y: position.y * PIXELS_PER_WALK_CELL + PIXELS_PER_WALK_CELL / 2,
    }
}

fn cross(a: PixelPosition, b: PixelPosition, point: PixelPosition) -> i64 {
    let delta_x = b.x as i64 - a.x as i64;
    let delta_y = b.y as i64 - a.y as i64;
    delta_x * (point.y as i64 - a.y as i64) - delta_y * (point.x as i64 - a.x as i64)
}

fn segments_intersect(
    a: PixelPosition,
    b: PixelPosition,
    c: PixelPosition,
    d: PixelPosition,
) -> bool {
    let abc = cross(a, b, c);
    let abd = cross(a, b, d);
    let cda = cross(c, d, a);
    let cdb = cross(c, d, b);
    if strictly_opposite(abc, abd) && strictly_opposite(cda, cdb) {
        return true;
    }
    (abc == 0 && on_segment(a, b, c))
        || (abd == 0 && on_segment(a, b, d))
        || (cda == 0 && on_segment(c, d, a))
        || (cdb == 0 && on_segment(c, d, b))
}

fn strictly_opposite(left: i64, right: i64) -> bool {
    (left < 0 && right > 0) || (left > 0 && right < 0)
}

fn on_segment(a: PixelPosition, b: PixelPosition, point: PixelPosition) -> bool {
    point.x >= a.x.min(b.x)
        && point.x <= a.x.max(b.x)
        && point.y >= a.y.min(b.y)
        && point.y <= a.y.max(b.y)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TerrainCell;

    fn grid(width: u32, height: u32, walkable: impl Fn(u32, u32) -> bool) -> TerrainGrid {
        TerrainGrid::from_cells(
            width,
            height,
            (0..height)
                .flat_map(|y| (0..width).map(move |x| (x, y)))
                .map(|(x, y)| TerrainCell {
                    walkable: walkable(x, y),
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                })
                .collect(),
        )
        .unwrap()
    }

    fn span(ax: u32, ay: u32, bx: u32, by: u32) -> BoundarySpan {
        BoundarySpan {
            endpoints: [
                PixelPosition { x: ax, y: ay },
                PixelPosition { x: bx, y: by },
            ],
        }
    }

    #[test]
    fn hall_span_splits_without_changing_routes() {
        let terrain = grid(9, 7, |x, y| {
            y == 3 || (x < 3 && (1..=5).contains(&y)) || (x > 5 && (1..=5).contains(&y))
        });
        let before_cells = terrain.cells().to_vec();
        let before_route = terrain
            .route(WalkPosition { x: 1, y: 3 }, WalkPosition { x: 7, y: 3 })
            .unwrap();
        let partition = terrain.partition_by_spans(&[span(36, 20, 36, 36)]).unwrap();
        assert_eq!(partition.areas().len(), 2);
        assert_eq!(partition.labels()[3 * 9 + 1], 1);
        assert_eq!(partition.labels()[3 * 9 + 7], 2);
        assert!(partition.boundaries()[0].separated_edge_count > 0);
        assert_eq!(partition.boundaries()[0].incident_area_ids, vec![1, 2]);
        assert_eq!(terrain.cells(), before_cells);
        assert_eq!(
            terrain
                .route(WalkPosition { x: 1, y: 3 }, WalkPosition { x: 7, y: 3 })
                .unwrap(),
            before_route
        );
    }

    #[test]
    fn dangling_span_in_open_room_can_be_bypassed() {
        let terrain = grid(7, 7, |_, _| true);
        let partition = terrain.partition_by_spans(&[span(28, 8, 28, 40)]).unwrap();
        assert_eq!(partition.areas().len(), 1);
        assert!(partition.boundaries()[0].removed_edge_count > 0);
        assert_eq!(partition.boundaries()[0].separated_edge_count, 0);
        assert!(partition.boundaries()[0].region_pairs.is_empty());
        assert_eq!(partition.boundaries()[0].incident_area_ids, vec![1]);
    }

    #[test]
    fn span_without_removed_edges_has_no_incident_areas() {
        let terrain = grid(3, 3, |_, _| false);
        let partition = terrain.partition_by_spans(&[span(8, 0, 8, 24)]).unwrap();
        let boundary = &partition.boundaries()[0];
        assert_eq!(boundary.removed_edge_count, 0);
        assert_eq!(boundary.separated_edge_count, 0);
        assert!(boundary.region_pairs.is_empty());
        assert!(boundary.incident_area_ids.is_empty());
    }

    #[test]
    fn two_exits_are_only_separated_together() {
        let terrain = grid(7, 5, |x, y| x != 3 || y == 1 || y == 3);
        let top = span(28, 4, 28, 20);
        let bottom = span(28, 20, 28, 36);
        assert_eq!(terrain.partition_by_spans(&[top]).unwrap().areas().len(), 1);
        assert_eq!(
            terrain.partition_by_spans(&[bottom]).unwrap().areas().len(),
            1
        );
        let partition = terrain.partition_by_spans(&[top, bottom]).unwrap();
        assert_eq!(partition.areas().len(), 2);
        assert!(
            partition
                .boundaries()
                .iter()
                .all(|boundary| boundary.separated_edge_count > 0)
        );
    }

    #[test]
    fn diagonal_span_cuts_diagonal_edges_without_endpoint_leak() {
        let terrain = grid(6, 6, |_, _| true);
        // Offset from every cell center so the side convention cannot provide a line of
        // zero-side vertices that bridges the two sides.
        let partition = terrain.partition_by_spans(&[span(0, 2, 46, 48)]).unwrap();
        assert_eq!(partition.areas().len(), 2);
        assert_ne!(partition.labels()[1], partition.labels()[6]);
        assert_ne!(partition.labels()[4 * 6], partition.labels()[5 * 6 + 5]);
    }

    #[test]
    fn line_through_centers_does_not_isolate_center_row() {
        let terrain = grid(7, 5, |_, _| true);
        let partition = terrain.partition_by_spans(&[span(28, 0, 28, 40)]).unwrap();
        assert_eq!(partition.areas().len(), 2);
        let labels = partition.labels();
        for y in 0..5 {
            assert_eq!(labels[y * 7 + 2], labels[y * 7 + 3]);
        }
    }

    #[test]
    fn input_order_reversal_and_duplicates_do_not_change_labels() {
        let terrain = grid(7, 5, |_, _| true);
        let left = span(28, 0, 28, 40);
        let right = span(28, 40, 28, 0);
        let horizontal = span(0, 20, 56, 20);
        let baseline = terrain.partition_by_spans(&[left, horizontal]).unwrap();
        let reordered = terrain
            .partition_by_spans(&[horizontal, right, left])
            .unwrap();
        assert_eq!(baseline.labels(), reordered.labels());
        assert_eq!(reordered.boundaries().len(), 3);
    }

    #[test]
    fn rejects_invalid_spans_and_limit() {
        let terrain = grid(2, 2, |_, _| true);
        assert_eq!(
            terrain.partition_by_spans(&[span(0, 0, 0, 0)]),
            Err(AreaError::ZeroLengthSpan {
                span: 0,
                x: 0,
                y: 0
            })
        );
        assert_eq!(
            terrain.partition_by_spans(&[span(0, 0, 17, 0)]),
            Err(AreaError::EndpointOutOfBounds {
                span: 0,
                endpoint: 1,
                x: 17,
                y: 0,
                width_pixels: 16,
                height_pixels: 16,
            })
        );
        let spans = vec![span(0, 0, 1, 1); MAX_BOUNDARY_SPANS + 1];
        assert_eq!(
            terrain.partition_by_spans(&spans),
            Err(AreaError::TooManyBoundarySpans { actual: 257 })
        );
    }

    #[test]
    fn every_walkable_cell_has_a_positive_label_and_count() {
        let terrain = grid(4, 3, |x, y| (x + y) % 3 != 0);
        let partition = terrain.partition_by_spans(&[]).unwrap();
        for (cell, label) in terrain.cells().iter().zip(partition.labels()) {
            assert_eq!(cell.walkable, *label != 0);
        }
        assert_eq!(
            partition
                .areas()
                .iter()
                .map(|area| area.cell_count)
                .sum::<u32>(),
            terrain.cells().iter().filter(|cell| cell.walkable).count() as u32
        );
    }

    #[test]
    fn cardinal_and_diagonal_geometry_matches_a_small_oracle() {
        let horizontal = SpanGeometry::new(span(0, 12, 32, 12));
        let diagonal = SpanGeometry::new(span(0, 0, 32, 32));
        assert!(horizontal.cuts(WalkPosition { x: 1, y: 0 }, WalkPosition { x: 1, y: 1 }));
        assert!(!horizontal.cuts(WalkPosition { x: 1, y: 1 }, WalkPosition { x: 1, y: 2 }));
        assert!(diagonal.cuts(WalkPosition { x: 1, y: 0 }, WalkPosition { x: 0, y: 1 }));
        assert!(!diagonal.cuts(WalkPosition { x: 1, y: 1 }, WalkPosition { x: 2, y: 2 }));
    }

    #[test]
    fn diagonal_candidate_work_is_linear_in_span_length() {
        let spans: Vec<_> = (0..MAX_BOUNDARY_SPANS as u32)
            .map(|offset| {
                let first = (0, offset);
                let second = (8_192, 8_192 - offset);
                if offset.is_multiple_of(2) {
                    span(first.0, first.1, second.0, second.1)
                } else {
                    span(second.0, second.1, first.0, first.1)
                }
            })
            .collect();
        let mut visited = 0;
        for span in spans {
            let candidates = candidate_positions(SpanGeometry::new(span), 1_024, 1_024);
            let unique: std::collections::HashSet<_> = candidates.iter().copied().collect();
            assert_eq!(unique.len(), candidates.len());
            visited += candidates.len();
        }
        assert!(
            visited <= MAX_BOUNDARY_SPANS * 1_024 * 7,
            "visited {visited} candidate cells"
        );
    }
}
