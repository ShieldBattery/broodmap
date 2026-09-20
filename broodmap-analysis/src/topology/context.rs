use super::{TopologyBase, TopologyBoundary};
use crate::{BoundaryAssessment, PixelPosition, SpanPartition, TerrainGrid};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone)]
pub(crate) struct RegionStats {
    pub cells: usize,
    pub min_x: f64,
    pub min_y: f64,
    pub max_x: f64,
    pub max_y: f64,
    pub has_ramp: bool,
    pub elevations: u8,
    pub elevation_counts: [usize; 4],
}
impl RegionStats {
    pub fn corners(&self) -> [[f64; 2]; 4] {
        [
            [self.min_x, self.min_y],
            [self.min_x, self.max_y],
            [self.max_x, self.min_y],
            [self.max_x, self.max_y],
        ]
    }
}

/// One graph/statistics view per partition; rebuilding after a removal refreshes every gate.
pub(crate) struct PartitionContext<'a> {
    pub grid: &'a TerrainGrid,
    pub partition: &'a SpanPartition,
    pub anchored: BTreeSet<u32>,
    pub incidents: BTreeMap<u32, BTreeSet<usize>>,
    pub stats: BTreeMap<u32, RegionStats>,
}
impl<'a> PartitionContext<'a> {
    pub fn new(
        grid: &'a TerrainGrid,
        partition: &'a SpanPartition,
        bases: &[TopologyBase],
    ) -> Self {
        let mut incidents: BTreeMap<u32, BTreeSet<usize>> = BTreeMap::new();
        for (index, boundary) in partition.boundaries().iter().enumerate() {
            for pair in &boundary.region_pairs {
                for region in pair {
                    incidents.entry(*region).or_default().insert(index);
                }
            }
        }
        let anchored = bases
            .iter()
            .filter_map(|base| base.route_anchor)
            .map(|p| partition.labels()[(p.y * grid.width() + p.x) as usize])
            .collect();
        let mut stats: BTreeMap<u32, RegionStats> = BTreeMap::new();
        for (index, &region) in partition.labels().iter().enumerate() {
            if region == 0 {
                continue;
            }
            let cell = grid.cells()[index];
            let x = (index % grid.width() as usize) as f64 * 8.0;
            let y = (index / grid.width() as usize) as f64 * 8.0;
            let value = stats.entry(region).or_insert(RegionStats {
                cells: 0,
                min_x: f64::INFINITY,
                min_y: f64::INFINITY,
                max_x: f64::NEG_INFINITY,
                max_y: f64::NEG_INFINITY,
                has_ramp: false,
                elevations: 0,
                elevation_counts: [0; 4],
            });
            value.cells += 1;
            value.min_x = value.min_x.min(x);
            value.min_y = value.min_y.min(y);
            value.max_x = value.max_x.max(x + 8.0);
            value.max_y = value.max_y.max(y + 8.0);
            value.has_ramp |= cell.ramp;
            value.elevations |= 1 << cell.elevation;
            value.elevation_counts[cell.elevation as usize] += 1;
        }
        Self {
            grid,
            partition,
            anchored,
            incidents,
            stats,
        }
    }
}

pub(crate) fn full_cut(entry: &BoundaryAssessment) -> bool {
    entry.removed_edge_count > 0
        && entry.removed_edge_count == entry.separated_edge_count
        && !entry.region_pairs.is_empty()
        && entry.region_pairs.iter().all(|p| p[0] != p[1])
}
pub(crate) fn full_pair(entry: &BoundaryAssessment) -> Option<[u32; 2]> {
    (full_cut(entry) && entry.region_pairs.len() == 1).then(|| entry.region_pairs[0])
}
pub(crate) fn canonical(mut endpoints: [PixelPosition; 2]) -> [PixelPosition; 2] {
    endpoints.sort();
    endpoints
}
pub(crate) fn point(point: PixelPosition) -> [f64; 2] {
    [f64::from(point.x), f64::from(point.y)]
}
pub(crate) fn center(boundary: &TopologyBoundary) -> [f64; 2] {
    let [a, b] = boundary.endpoints;
    [
        (f64::from(a.x) + f64::from(b.x)) / 2.0,
        (f64::from(a.y) + f64::from(b.y)) / 2.0,
    ]
}
pub(crate) fn distance(a: [f64; 2], b: [f64; 2]) -> f64 {
    (a[0] - b[0]).hypot(a[1] - b[1])
}
pub(crate) fn point_segment_distance(p: [f64; 2], a: [f64; 2], b: [f64; 2]) -> f64 {
    let dx = b[0] - a[0];
    let dy = b[1] - a[1];
    let n = dx * dx + dy * dy;
    let t = if n == 0.0 {
        0.0
    } else {
        (((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / n).clamp(0.0, 1.0)
    };
    distance(p, [a[0] + t * dx, a[1] + t * dy])
}

/// Normalize retained geometry without changing its supporting evidence.
pub(crate) fn canonical_boundary(mut boundary: TopologyBoundary) -> TopologyBoundary {
    boundary.endpoints = canonical(boundary.endpoints);
    boundary
}
pub(crate) fn compare_boundaries(
    left: &TopologyBoundary,
    right: &TopologyBoundary,
) -> std::cmp::Ordering {
    canonical(left.endpoints).cmp(&canonical(right.endpoints))
}
pub(crate) fn canonical_output(
    boundaries: &[TopologyBoundary],
    removed: &BTreeSet<usize>,
) -> Vec<TopologyBoundary> {
    let mut result: Vec<_> = boundaries
        .iter()
        .enumerate()
        .filter(|(index, _)| !removed.contains(index))
        .map(|(_, boundary)| canonical_boundary(boundary.clone()))
        .collect();
    result.sort_by(compare_boundaries);
    result
}
