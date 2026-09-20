//! Conservative selectors for redundant route-local ramp boundary evidence.
//!
//! These rules retain the finite spans that were observed. They only remove an ordinary span
//! when the partition evidence shows it is a local ramp mouth or sealed ramp interior.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use super::context::{
    PartitionContext, canonical, canonical_output, center, distance, full_cut, full_pair, point,
    point_segment_distance,
};
use super::{RampSide, TopologyBoundary};

const MAX_MOUTH_DISTANCE: f64 = 224.0;
const MAX_APPROACH_RADIUS: f64 = 512.0;
const MAX_REPRESENTATIVE_OFFSET: f64 = 32.0;
const MAX_OVERLAY_NORMAL_OFFSET: f64 = 64.0;
const MIN_OVERLAY_FRACTION: f64 = 0.75;
const MAX_OVERLAY_WIDTH_RATIO: f64 = 1.5;
const MIN_PARALLEL_COSINE: f64 = 0.965_925_826_289_068_3; // cos(PI / 12)

#[derive(Clone)]
enum MouthShape {
    Single([u32; 2]),
    Star { outer: u32, inner: Vec<u32> },
}

struct MouthProposal {
    mouth_index: usize,
    ramp_index: usize,
    outer: Option<u32>,
    inner: Vec<u32>,
    union: Vec<u32>,
    triangle: bool,
    separation: f64,
    ramp_center: [f64; 2],
    mouth_center: [f64; 2],
    two_port_broad_approach: bool,
}

struct InteriorProposal {
    regions: Vec<u32>,
    edges: Vec<usize>,
    ports: [usize; 2],
    ramp_id: u32,
}

fn endpoints(boundary: &TopologyBoundary) -> [crate::PixelPosition; 2] {
    canonical(boundary.endpoints)
}

fn is_nearby_parallel_representative(
    left: [crate::PixelPosition; 2],
    right: [crate::PixelPosition; 2],
) -> bool {
    let [left_start, left_end] = canonical(left);
    let [right_start, right_end] = canonical(right);
    if distance(point(left_start), point(right_start)) > MAX_REPRESENTATIVE_OFFSET
        || distance(point(left_end), point(right_end)) > MAX_REPRESENTATIVE_OFFSET
    {
        return false;
    }
    let [left_x, left_y] = [
        f64::from(left_end.x) - f64::from(left_start.x),
        f64::from(left_end.y) - f64::from(left_start.y),
    ];
    let [right_x, right_y] = [
        f64::from(right_end.x) - f64::from(right_start.x),
        f64::from(right_end.y) - f64::from(right_start.y),
    ];
    let lengths = left_x.hypot(left_y) * right_x.hypot(right_y);
    lengths > 0.0 && (left_x * right_x + left_y * right_y).abs() / lengths >= MIN_PARALLEL_COSINE
}

fn overlapping_ramp_representative(
    candidate: [crate::PixelPosition; 2],
    ramp: [crate::PixelPosition; 2],
) -> Option<f64> {
    let [candidate_start, candidate_end] = canonical(candidate);
    let [ramp_start, ramp_end] = canonical(ramp);
    let [dx, dy] = [
        f64::from(ramp_end.x) - f64::from(ramp_start.x),
        f64::from(ramp_end.y) - f64::from(ramp_start.y),
    ];
    let ramp_length = dx.hypot(dy);
    let [candidate_dx, candidate_dy] = [
        f64::from(candidate_end.x) - f64::from(candidate_start.x),
        f64::from(candidate_end.y) - f64::from(candidate_start.y),
    ];
    let candidate_length = candidate_dx.hypot(candidate_dy);
    if ramp_length == 0.0
        || candidate_length == 0.0
        || (dx * candidate_dx + dy * candidate_dy).abs() / (ramp_length * candidate_length)
            < MIN_PARALLEL_COSINE
    {
        return None;
    }
    let normal_offset = |position: crate::PixelPosition| {
        ((f64::from(position.x) - f64::from(ramp_start.x)) * dy
            - (f64::from(position.y) - f64::from(ramp_start.y)) * dx)
            .abs()
            / ramp_length
    };
    let max_offset = normal_offset(candidate_start).max(normal_offset(candidate_end));
    if max_offset > MAX_OVERLAY_NORMAL_OFFSET {
        return None;
    }
    let project = |position: crate::PixelPosition| {
        ((f64::from(position.x) - f64::from(ramp_start.x)) * dx
            + (f64::from(position.y) - f64::from(ramp_start.y)) * dy)
            / ramp_length
    };
    let candidate_min = project(candidate_start).min(project(candidate_end));
    let candidate_max = project(candidate_start).max(project(candidate_end));
    let overlap = (ramp_length.min(candidate_max) - 0.0_f64.max(candidate_min)).max(0.0);
    (overlap >= MIN_OVERLAY_FRACTION * ramp_length.min(candidate_length)).then_some(max_offset)
}

fn orientation(
    first: crate::PixelPosition,
    second: crate::PixelPosition,
    third: crate::PixelPosition,
) -> i128 {
    (i128::from(second.x) - i128::from(first.x)) * (i128::from(third.y) - i128::from(first.y))
        - (i128::from(second.y) - i128::from(first.y)) * (i128::from(third.x) - i128::from(first.x))
}

fn on_segment(
    first: crate::PixelPosition,
    second: crate::PixelPosition,
    point: crate::PixelPosition,
) -> bool {
    first.x.min(second.x) <= point.x
        && point.x <= first.x.max(second.x)
        && first.y.min(second.y) <= point.y
        && point.y <= first.y.max(second.y)
}

fn intersects(left: [crate::PixelPosition; 2], right: [crate::PixelPosition; 2]) -> bool {
    let [left_start, left_end] = left;
    let [right_start, right_end] = right;
    let left_right_start = orientation(left_start, left_end, right_start);
    let left_right_end = orientation(left_start, left_end, right_end);
    let right_left_start = orientation(right_start, right_end, left_start);
    let right_left_end = orientation(right_start, right_end, left_end);
    (left_right_start == 0 && on_segment(left_start, left_end, right_start))
        || (left_right_end == 0 && on_segment(left_start, left_end, right_end))
        || (right_left_start == 0 && on_segment(right_start, right_end, left_start))
        || (right_left_end == 0 && on_segment(right_start, right_end, left_end))
        || (left_right_start.signum() != left_right_end.signum()
            && right_left_start.signum() != right_left_end.signum())
}

fn segment_distance(left: [crate::PixelPosition; 2], right: [crate::PixelPosition; 2]) -> f64 {
    if intersects(left, right) {
        return 0.0;
    }
    let [left_start, left_end] = left.map(point);
    let [right_start, right_end] = right.map(point);
    point_segment_distance(left_start, right_start, right_end)
        .min(point_segment_distance(left_end, right_start, right_end))
        .min(point_segment_distance(right_start, left_start, left_end))
        .min(point_segment_distance(right_end, left_start, left_end))
}

fn star(entry: &crate::BoundaryAssessment) -> Option<MouthShape> {
    if !full_cut(entry) {
        return None;
    }
    if entry.region_pairs.len() == 1 {
        return Some(MouthShape::Single(entry.region_pairs[0]));
    }
    let common: Vec<u32> = entry.region_pairs[0]
        .into_iter()
        .filter(|region| entry.region_pairs.iter().all(|pair| pair.contains(region)))
        .collect();
    let [outer] = common.as_slice() else {
        return None;
    };
    let mut inner = Vec::new();
    for pair in &entry.region_pairs {
        for region in pair {
            if *region != *outer && !inner.contains(region) {
                inner.push(*region);
            }
        }
    }
    (!inner.is_empty()).then_some(MouthShape::Star {
        outer: *outer,
        inner,
    })
}

fn triangle_representative(
    mouth: &crate::BoundaryAssessment,
    ramp: &crate::BoundaryAssessment,
) -> Option<Vec<u32>> {
    if !full_cut(mouth)
        || !full_cut(ramp)
        || mouth.region_pairs.len() != 2
        || ramp.region_pairs.len() != 2
    {
        return None;
    }
    let mut regions = Vec::new();
    for pair in &mouth.region_pairs {
        for region in pair {
            if !regions.contains(region) {
                regions.push(*region);
            }
        }
    }
    if regions.len() != 3
        || regions.iter().any(|region| {
            !ramp
                .region_pairs
                .iter()
                .flatten()
                .any(|ramp_region| ramp_region == region)
        })
    {
        return None;
    }
    let edge_key = |pair: [u32; 2]| {
        if pair[0] <= pair[1] {
            pair
        } else {
            [pair[1], pair[0]]
        }
    };
    let mouth_edges: BTreeSet<_> = mouth.region_pairs.iter().copied().map(edge_key).collect();
    let ramp_edges: BTreeSet<_> = ramp.region_pairs.iter().copied().map(edge_key).collect();
    let edges: BTreeSet<_> = mouth_edges.union(&ramp_edges).copied().collect();
    if mouth_edges.len() != 2
        || ramp_edges.len() != 2
        || edges.len() != 3
        || mouth_edges.intersection(&ramp_edges).count() != 1
    {
        return None;
    }
    regions.sort_unstable();
    Some(regions)
}

/// Retains structural ramp ends while removing a local ordinary widening that represents a ramp
/// mouth. The returned spans are canonical and sorted; all retained evidence is cloned unchanged.
pub(super) fn replace_ramp_mouths(
    boundaries: &[TopologyBoundary],
    ctx: &PartitionContext<'_>,
) -> Vec<TopologyBoundary> {
    if ctx.partition.boundaries().len() != boundaries.len() {
        return canonical_output(boundaries, &BTreeSet::new());
    }

    let metadata = ctx.partition.boundaries();
    let mut proposals = Vec::new();
    for (ramp_index, ramp) in boundaries.iter().enumerate() {
        let Some(ramp_provenance) = ramp.ramp else {
            continue;
        };
        if !full_cut(&metadata[ramp_index]) {
            continue;
        }
        let ramp_regions = full_pair(&metadata[ramp_index]);
        for (mouth_index, mouth) in boundaries.iter().enumerate() {
            let nearby_representative =
                is_nearby_parallel_representative(mouth.endpoints, ramp.endpoints);
            let overlay_representative = mouth.width_pixels.max(ramp.width_pixels)
                <= MAX_OVERLAY_WIDTH_RATIO * mouth.width_pixels.min(ramp.width_pixels)
                && overlapping_ramp_representative(mouth.endpoints, ramp.endpoints).is_some();
            let broad_mouth = mouth.width_pixels >= 2.0 * ramp.width_pixels;
            let mouth_shape = star(&metadata[mouth_index]);
            let triangle = nearby_representative
                .then(|| triangle_representative(&metadata[mouth_index], &metadata[ramp_index]))
                .flatten();
            if mouth.ramp.is_some()
                || (mouth_shape.is_none() && triangle.is_none())
                || (!broad_mouth && !nearby_representative && !overlay_representative)
            {
                continue;
            }

            let (outer, inner, union, triangle, two_port_broad_approach) = if let Some(triangle) =
                triangle
            {
                if triangle
                    .iter()
                    .filter(|region| ctx.anchored.contains(region))
                    .count()
                    > 1
                {
                    continue;
                }
                let inner = triangle
                    .iter()
                    .copied()
                    .filter(|region| !ctx.anchored.contains(region))
                    .collect();
                let outer = triangle
                    .iter()
                    .copied()
                    .find(|region| ctx.anchored.contains(region));
                (outer, inner, triangle, true, false)
            } else {
                let Some(ramp_regions) = ramp_regions else {
                    continue;
                };
                let mouth_shape = mouth_shape.expect("checked above");
                let mouth_inner = match &mouth_shape {
                    MouthShape::Single(pair) => pair.as_slice(),
                    MouthShape::Star { inner, .. } => inner.as_slice(),
                };
                let mouth_has_anchored_region = mouth_inner
                    .iter()
                    .any(|region| ctx.anchored.contains(region));
                let single_mouth = matches!(mouth_shape, MouthShape::Single(_));
                let shared: Vec<_> = mouth_inner
                    .iter()
                    .copied()
                    .filter(|region| ramp_regions.contains(region))
                    .collect();
                if shared.len() != 1 {
                    continue;
                }
                let approach = shared[0];
                let (outer, inner) = match mouth_shape {
                    MouthShape::Single(pair) => (
                        pair.into_iter().find(|region| *region != approach),
                        vec![approach],
                    ),
                    MouthShape::Star { outer, inner } => (Some(outer), inner),
                };
                let Some(outer) = outer else {
                    continue;
                };
                let union = std::iter::once(outer)
                    .chain(inner.iter().copied())
                    .collect::<Vec<_>>();
                if inner.iter().any(|region| ctx.anchored.contains(region)) {
                    continue;
                }
                let approach_incidents = ctx.incidents.get(&approach);
                let overlay_only = overlay_representative && !nearby_representative && !broad_mouth;
                if overlay_only
                    && (!single_mouth
                        || mouth_has_anchored_region
                        || approach_incidents.is_none_or(|incidents| {
                            incidents.len() != 2
                                || !incidents.contains(&mouth_index)
                                || !incidents.contains(&ramp_index)
                        }))
                {
                    continue;
                }
                let mut two_port_broad_approach = (broad_mouth || overlay_only)
                    && single_mouth
                    && approach_incidents.is_some_and(|incidents| {
                        incidents.len() == 2
                            && incidents.contains(&mouth_index)
                            && incidents.contains(&ramp_index)
                    });
                if boundaries.iter().enumerate().any(|(index, other)| {
                    other.ramp.is_some_and(|other_ramp| {
                        other_ramp.id == ramp_provenance.id
                            && other_ramp.end != ramp_provenance.end
                            && metadata[index]
                                .region_pairs
                                .iter()
                                .any(|pair| pair.contains(&approach))
                    })
                }) {
                    continue;
                }
                if inner.len() > 1 {
                    if approach_incidents.is_none_or(|incidents| {
                        incidents.len() != 2
                            || !incidents.contains(&mouth_index)
                            || !incidents.contains(&ramp_index)
                    }) || inner.iter().any(|region| {
                        *region != approach
                            && ctx.incidents.get(region).is_none_or(|incidents| {
                                incidents.len() != 1 || !incidents.contains(&mouth_index)
                            })
                    }) {
                        continue;
                    }
                    two_port_broad_approach = broad_mouth;
                }
                (Some(outer), inner, union, false, two_port_broad_approach)
            };
            let separation = segment_distance(mouth.endpoints, ramp.endpoints);
            if separation > MAX_MOUTH_DISTANCE {
                continue;
            }
            proposals.push(MouthProposal {
                mouth_index,
                ramp_index,
                outer,
                inner,
                union,
                triangle,
                separation,
                ramp_center: center(ramp),
                mouth_center: center(mouth),
                two_port_broad_approach,
            });
        }
    }

    proposals.retain(|proposal| {
        proposal.inner.iter().all(|region| {
            let Some(stat) = ctx.stats.get(region) else {
                return false;
            };
            let centers = if proposal.two_port_broad_approach {
                [Some(proposal.ramp_center), Some(proposal.mouth_center)]
            } else {
                [Some(proposal.ramp_center), None]
            };
            stat.min_x.is_finite()
                && stat.corners().iter().all(|corner| {
                    centers.iter().flatten().any(|candidate_center| {
                        distance(*corner, *candidate_center) <= MAX_APPROACH_RADIUS
                    })
                })
        })
    });
    proposals.sort_by(|left, right| {
        left.separation
            .partial_cmp(&right.separation)
            .unwrap_or(Ordering::Equal)
            .then_with(|| {
                endpoints(&boundaries[left.mouth_index])
                    .cmp(&endpoints(&boundaries[right.mouth_index]))
            })
            .then_with(|| {
                endpoints(&boundaries[left.ramp_index])
                    .cmp(&endpoints(&boundaries[right.ramp_index]))
            })
    });

    let mut removed = BTreeSet::new();
    let mut absorbed = BTreeSet::new();
    let mut outers = BTreeSet::new();
    let mut ramp_ends = BTreeSet::new();
    let mut occupied = BTreeSet::new();
    let mut triangle_reserved = BTreeSet::new();
    for proposal in proposals {
        let ramp = boundaries[proposal.ramp_index]
            .ramp
            .expect("ramp proposal must retain provenance");
        if removed.contains(&proposal.mouth_index)
            || ramp_ends.contains(&(ramp.id, ramp.end))
            || proposal
                .outer
                .is_some_and(|outer| absorbed.contains(&outer))
            || proposal
                .inner
                .iter()
                .any(|region| absorbed.contains(region) || outers.contains(region))
            || (proposal.triangle
                && proposal
                    .union
                    .iter()
                    .any(|region| occupied.contains(region)))
            || (!proposal.triangle
                && proposal
                    .union
                    .iter()
                    .any(|region| triangle_reserved.contains(region)))
        {
            continue;
        }
        removed.insert(proposal.mouth_index);
        absorbed.extend(proposal.inner);
        if let Some(outer) = proposal.outer {
            outers.insert(outer);
        }
        occupied.extend(proposal.union.iter().copied());
        if proposal.triangle {
            triangle_reserved.extend(proposal.union);
        }
        ramp_ends.insert((ramp.id, ramp.end));
    }
    canonical_output(boundaries, &removed)
}

/// Collapses an ordinary-cut component only when both ends of one ramp seal that component.
pub(super) fn collapse_ramp_interiors(
    boundaries: &[TopologyBoundary],
    ctx: &PartitionContext<'_>,
) -> Vec<TopologyBoundary> {
    if ctx.partition.boundaries().len() != boundaries.len() {
        return canonical_output(boundaries, &BTreeSet::new());
    }
    let metadata = ctx.partition.boundaries();
    let mut graph: BTreeMap<u32, Vec<(u32, usize)>> = BTreeMap::new();
    for (index, entry) in metadata.iter().enumerate() {
        if boundaries[index].ramp.is_none() && full_cut(entry) {
            for [left, right] in &entry.region_pairs {
                graph.entry(*left).or_default().push((*right, index));
                graph.entry(*right).or_default().push((*left, index));
            }
        }
    }

    let mut visited = BTreeSet::new();
    let mut proposals = Vec::new();
    for start in graph.keys().copied().collect::<Vec<_>>() {
        if !visited.insert(start) {
            continue;
        }
        let mut regions = BTreeSet::from([start]);
        let mut edges = BTreeSet::new();
        let mut todo = vec![start];
        while let Some(region) = todo.pop() {
            for &(neighbor, index) in &graph[&region] {
                edges.insert(index);
                if visited.insert(neighbor) {
                    regions.insert(neighbor);
                    todo.push(neighbor);
                }
            }
        }
        if edges.is_empty()
            || edges.len() > 8
            || regions.iter().any(|region| ctx.anchored.contains(region))
        {
            continue;
        }
        if edges.iter().any(|index| {
            metadata[*index]
                .region_pairs
                .iter()
                .any(|pair| pair.iter().any(|region| !regions.contains(region)))
        }) {
            continue;
        }
        let mut ports = BTreeSet::new();
        for region in &regions {
            for index in ctx.incidents.get(region).into_iter().flatten() {
                if !edges.contains(index) {
                    ports.insert(*index);
                }
            }
        }
        let port_indexes: Vec<_> = ports.into_iter().collect();
        let [first_port, second_port] = port_indexes.as_slice() else {
            continue;
        };
        let port_indexes = [*first_port, *second_port];
        let Some(first_ramp) = boundaries[port_indexes[0]].ramp else {
            continue;
        };
        let Some(second_ramp) = boundaries[port_indexes[1]].ramp else {
            continue;
        };
        let ends = BTreeSet::from([first_ramp.end, second_ramp.end]);
        if !full_cut(&metadata[port_indexes[0]])
            || !full_cut(&metadata[port_indexes[1]])
            || first_ramp.id != second_ramp.id
            || ends != BTreeSet::from([RampSide::Lower, RampSide::Upper])
        {
            continue;
        }
        let mut expanded_regions = regions;
        let mut valid_ports = true;
        for index in port_indexes {
            let pairs = &metadata[index].region_pairs;
            let mut exteriors = BTreeSet::new();
            for pair in pairs {
                let inside: Vec<_> = pair
                    .iter()
                    .copied()
                    .filter(|region| expanded_regions.contains(region))
                    .collect();
                if inside.len() > 1 {
                    valid_ports = false;
                }
                if let [inside] = inside.as_slice() {
                    exteriors.insert(
                        pair.iter()
                            .copied()
                            .find(|region| region != inside)
                            .expect("pair has two regions"),
                    );
                }
            }
            for pair in pairs {
                if pair.iter().any(|region| expanded_regions.contains(region)) {
                    continue;
                }
                let exterior: Vec<_> = pair
                    .iter()
                    .copied()
                    .filter(|region| exteriors.contains(region))
                    .collect();
                let [exterior] = exterior.as_slice() else {
                    valid_ports = false;
                    continue;
                };
                let leaf = pair
                    .iter()
                    .copied()
                    .find(|region| region != exterior)
                    .expect("pair has two regions");
                if ctx.anchored.contains(&leaf)
                    || ctx
                        .incidents
                        .get(&leaf)
                        .is_none_or(|incidents| incidents.len() != 1 || !incidents.contains(&index))
                {
                    valid_ports = false;
                    continue;
                }
                expanded_regions.insert(leaf);
            }
            if pairs.iter().any(|pair| {
                pair.iter()
                    .filter(|region| expanded_regions.contains(region))
                    .count()
                    != 1
            }) {
                valid_ports = false;
            }
        }
        if valid_ports {
            proposals.push(InteriorProposal {
                regions: expanded_regions.into_iter().collect(),
                edges: edges.into_iter().collect(),
                ports: port_indexes,
                ramp_id: first_ramp.id,
            });
        }
    }

    proposals.retain(|proposal| {
        let ports = proposal.ports.map(|index| &boundaries[index]);
        let centers = ports.map(center);
        let ramp = ports[0].ramp.expect("validated ramp port");
        let allowed_elevations = (1_u8 << ramp.lower_elevation) | (1_u8 << ramp.upper_elevation);
        proposal
            .regions
            .iter()
            .any(|region| ctx.stats.get(region).is_some_and(|stat| stat.has_ramp))
            && proposal.regions.iter().all(|region| {
                ctx.stats.get(region).is_some_and(|stat| {
                    stat.min_x.is_finite()
                        && stat.elevations & !allowed_elevations == 0
                        && stat.corners().iter().all(|corner| {
                            centers
                                .iter()
                                .any(|center| distance(*corner, *center) <= MAX_APPROACH_RADIUS)
                        })
                })
            })
    });
    proposals.sort_by(|left, right| {
        left.ramp_id.cmp(&right.ramp_id).then_with(|| {
            endpoints(&boundaries[left.ports[0]]).cmp(&endpoints(&boundaries[right.ports[0]]))
        })
    });

    let mut removed = BTreeSet::new();
    let mut reserved_regions = BTreeSet::new();
    for proposal in proposals {
        if proposal.edges.iter().any(|index| removed.contains(index))
            || proposal
                .regions
                .iter()
                .any(|region| reserved_regions.contains(region))
        {
            continue;
        }
        removed.extend(proposal.edges);
        reserved_regions.extend(proposal.regions);
    }
    canonical_output(boundaries, &removed)
}

#[cfg(test)]
mod tests {
    use super::super::context::{PartitionContext, canonical_output, full_cut};
    use super::super::{
        RampBoundary, RampSide, TopologyBase, TopologyBoundary, TopologyObservation,
    };
    use super::{
        MouthShape, collapse_ramp_interiors, is_nearby_parallel_representative,
        overlapping_ramp_representative, replace_ramp_mouths, segment_distance, star,
        triangle_representative,
    };
    use crate::{
        BoundaryAssessment, BoundarySpan, PixelPosition, SpanPartition, TerrainCell, TerrainGrid,
        WalkPosition,
    };
    use std::collections::BTreeSet;
    fn grid() -> TerrainGrid {
        TerrainGrid::from_cells(
            7,
            5,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                35
            ],
        )
        .unwrap()
    }

    fn reversed_ramp_boundary() -> TopologyBoundary {
        TopologyBoundary {
            endpoints: [
                PixelPosition { x: 28, y: 40 },
                PixelPosition { x: 28, y: 0 },
            ],
            width_pixels: 32.0,
            sources: vec![[9, 4]],
            observations: vec![TopologyObservation {
                endpoints: [
                    PixelPosition { x: 28, y: 40 },
                    PixelPosition { x: 28, y: 0 },
                ],
                width_pixels: 32.0,
                sources: vec![[9, 4]],
            }],
            ramp: Some(RampBoundary {
                id: 7,
                end: RampSide::Lower,
                lower_elevation: 0,
                upper_elevation: 1,
            }),
        }
    }

    #[test]
    fn selectors_keep_unselected_evidence_and_canonicalize_real_partition_boundaries() {
        let grid = grid();
        let partition = grid
            .partition_by_spans(&[BoundarySpan {
                endpoints: [
                    PixelPosition { x: 28, y: 0 },
                    PixelPosition { x: 28, y: 40 },
                ],
            }])
            .unwrap();
        assert!(full_cut(&partition.boundaries()[0]));
        let context = PartitionContext::new(&grid, &partition, &[]);
        let boundary = reversed_ramp_boundary();

        for selected in [
            replace_ramp_mouths(std::slice::from_ref(&boundary), &context),
            collapse_ramp_interiors(std::slice::from_ref(&boundary), &context),
        ] {
            assert_eq!(selected.len(), 1);
            assert_eq!(
                selected[0].endpoints,
                [
                    PixelPosition { x: 28, y: 0 },
                    PixelPosition { x: 28, y: 40 }
                ]
            );
            assert_eq!(selected[0].sources, boundary.sources);
            assert_eq!(selected[0].observations, boundary.observations);
            assert_eq!(selected[0].ramp, boundary.ramp);
        }
    }
    fn corridor_grid(ramp_cell: Option<(u32, u32, u8)>) -> TerrainGrid {
        let mut cells = vec![
            TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            64 * 64
        ];
        if let Some((x, y, elevation)) = ramp_cell {
            cells[(y * 64 + x) as usize] = TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation,
                ramp: true,
            };
        }
        TerrainGrid::from_cells(64, 64, cells).unwrap()
    }

    fn vertical(x: u32) -> BoundarySpan {
        BoundarySpan {
            endpoints: [PixelPosition { x, y: 0 }, PixelPosition { x, y: 512 }],
        }
    }

    fn ordinary(span: BoundarySpan, width_pixels: f64) -> TopologyBoundary {
        TopologyBoundary {
            endpoints: span.endpoints,
            width_pixels,
            sources: vec![[1, 2]],
            observations: vec![],
            ramp: None,
        }
    }

    fn ramp(span: BoundarySpan, id: u32, end: RampSide) -> TopologyBoundary {
        TopologyBoundary {
            endpoints: span.endpoints,
            width_pixels: 96.0,
            sources: vec![],
            observations: vec![],
            ramp: Some(RampBoundary {
                id,
                end,
                lower_elevation: 0,
                upper_elevation: 1,
            }),
        }
    }

    fn mouth_fixture(opposite_end: bool) -> (TerrainGrid, SpanPartition, Vec<TopologyBoundary>) {
        let grid = corridor_grid(None);
        let mouth = vertical(160);
        let ramp_end = vertical(200);
        let mut spans = vec![mouth, ramp_end];
        if opposite_end {
            spans.push(ramp_end);
        }
        let partition = grid.partition_by_spans(&spans).unwrap();
        let mut boundaries = vec![ordinary(mouth, 240.0), ramp(ramp_end, 7, RampSide::Lower)];
        if opposite_end {
            boundaries.push(ramp(ramp_end, 7, RampSide::Upper));
        }
        (grid, partition, boundaries)
    }

    fn interior_fixture(
        ramp_cell: Option<(u32, u32, u8)>,
        extra_port: bool,
    ) -> (TerrainGrid, SpanPartition, Vec<TopologyBoundary>) {
        let grid = corridor_grid(ramp_cell);
        let upper = vertical(160);
        let interior = vertical(200);
        let lower = vertical(240);
        let mut spans = vec![upper, interior, lower];
        if extra_port {
            spans.push(upper);
        }
        let partition = grid.partition_by_spans(&spans).unwrap();
        let mut boundaries = vec![
            ramp(upper, 19, RampSide::Upper),
            ordinary(interior, 200.0),
            ramp(lower, 19, RampSide::Lower),
        ];
        if extra_port {
            boundaries.push(ramp(upper, 91, RampSide::Lower));
        }
        (grid, partition, boundaries)
    }

    #[test]
    fn mouths_require_an_unanchored_two_port_approach_and_keep_structural_evidence() {
        let (grid, partition, boundaries) = mouth_fixture(false);
        assert!(partition.boundaries().iter().all(full_cut));
        let selected =
            replace_ramp_mouths(&boundaries, &PartitionContext::new(&grid, &partition, &[]));
        assert_eq!(selected, vec![boundaries[1].clone()]);

        let anchored = [TopologyBase {
            id: 1,
            route_anchor: Some(WalkPosition { x: 22, y: 32 }),
        }];
        assert_eq!(
            replace_ramp_mouths(
                &boundaries,
                &PartitionContext::new(&grid, &partition, &anchored),
            ),
            canonical_output(&boundaries, &BTreeSet::new())
        );

        let mut overlay = boundaries.clone();
        overlay[0].width_pixels = 96.0;
        // Another observation of the ramp span creates a real third incident cut record.
        overlay.push(ordinary(vertical(200), 96.0));
        let extra_partition = grid
            .partition_by_spans(
                &overlay
                    .iter()
                    .map(|b| BoundarySpan {
                        endpoints: b.endpoints,
                    })
                    .collect::<Vec<_>>(),
            )
            .unwrap();
        let context = PartitionContext::new(&grid, &extra_partition, &[]);
        assert_eq!(
            replace_ramp_mouths(&overlay, &context),
            canonical_output(&overlay, &BTreeSet::new())
        );
    }

    #[test]
    fn mouths_reject_remote_geometry_and_the_other_end_of_the_same_ramp() {
        let (grid, partition, mut boundaries) = mouth_fixture(false);
        boundaries[0].endpoints = vertical(480).endpoints;
        assert_eq!(
            replace_ramp_mouths(&boundaries, &PartitionContext::new(&grid, &partition, &[])),
            canonical_output(&boundaries, &BTreeSet::new())
        );

        let (grid, partition, boundaries) = mouth_fixture(true);
        assert_eq!(
            replace_ramp_mouths(&boundaries, &PartitionContext::new(&grid, &partition, &[])),
            canonical_output(&boundaries, &BTreeSet::new())
        );
    }

    #[test]
    fn parallel_representative_geometry_requires_local_parallel_overlap() {
        let points = |endpoints: [[u32; 2]; 2]| endpoints.map(|[x, y]| PixelPosition { x, y });
        let ramp_end = points([[80, 80], [240, 240]]);
        assert!(is_nearby_parallel_representative(
            points([[88, 88], [248, 248]]),
            ramp_end,
        ));
        assert!(!is_nearby_parallel_representative(
            points([[80, 100], [160, 100]]),
            points([[85, 80], [155, 120]]),
        ));
        assert!(!is_nearby_parallel_representative(
            points([[128, 128], [288, 288]]),
            ramp_end,
        ));
        assert!(
            overlapping_ramp_representative(points([[120, 40], [280, 200]]), ramp_end).is_some()
        );
        assert!(
            overlapping_ramp_representative(points([[128, 128], [288, 288]]), ramp_end).is_none()
        );
        assert!(
            overlapping_ramp_representative(points([[128, 32], [288, 192]]), ramp_end).is_none()
        );
    }

    #[test]
    fn multipart_mouth_shapes_and_triangles_require_a_closed_local_graph() {
        let full = |region_pairs| BoundaryAssessment {
            removed_edge_count: 1,
            separated_edge_count: 1,
            incident_area_ids: vec![],
            region_pairs,
        };
        assert!(matches!(
            star(&full(vec![[1, 2], [1, 3]])),
            Some(MouthShape::Star { outer: 1, inner }) if inner == vec![2, 3]
        ));
        assert!(star(&full(vec![[1, 2], [3, 4]])).is_none());

        let mouth = full(vec![[1, 3], [2, 3]]);
        let ramp = full(vec![[1, 2], [1, 3]]);
        assert_eq!(triangle_representative(&mouth, &ramp), Some(vec![1, 2, 3]));
        assert!(triangle_representative(&mouth, &full(vec![[1, 2], [1, 4]])).is_none());
    }

    #[test]
    fn ramp_interiors_require_ramp_and_allowed_elevation_evidence() {
        let (flat_grid, partition, boundaries) = interior_fixture(None, false);
        assert!(partition.boundaries().iter().all(full_cut));
        assert_eq!(
            collapse_ramp_interiors(
                &boundaries,
                &PartitionContext::new(&flat_grid, &partition, &[]),
            ),
            canonical_output(&boundaries, &BTreeSet::new())
        );

        let (grid, partition, boundaries) = interior_fixture(Some((22, 32, 1)), false);
        let selected =
            collapse_ramp_interiors(&boundaries, &PartitionContext::new(&grid, &partition, &[]));
        assert_eq!(selected, vec![boundaries[0].clone(), boundaries[2].clone()]);

        let (wrong_grid, partition, boundaries) = interior_fixture(Some((22, 32, 2)), false);
        assert_eq!(
            collapse_ramp_interiors(
                &boundaries,
                &PartitionContext::new(&wrong_grid, &partition, &[]),
            ),
            canonical_output(&boundaries, &BTreeSet::new())
        );
    }

    #[test]
    fn ramp_interiors_reject_anchors_and_a_third_structural_port() {
        let (grid, partition, boundaries) = interior_fixture(Some((22, 32, 1)), false);
        let anchored = [TopologyBase {
            id: 1,
            route_anchor: Some(WalkPosition { x: 22, y: 32 }),
        }];
        assert_eq!(
            collapse_ramp_interiors(
                &boundaries,
                &PartitionContext::new(&grid, &partition, &anchored),
            ),
            canonical_output(&boundaries, &BTreeSet::new())
        );

        let (grid, partition, boundaries) = interior_fixture(Some((22, 32, 1)), true);
        assert_eq!(
            collapse_ramp_interiors(&boundaries, &PartitionContext::new(&grid, &partition, &[]),),
            canonical_output(&boundaries, &BTreeSet::new())
        );
    }

    #[test]
    fn finite_segment_distance_handles_crossings_diagonal_gaps_and_points() {
        let points = |endpoints: [[u32; 2]; 2]| endpoints.map(|[x, y]| PixelPosition { x, y });
        assert_eq!(
            segment_distance(points([[0, 0], [8, 8]]), points([[0, 8], [8, 0]])),
            0.0
        );
        assert_eq!(
            segment_distance(points([[0, 0], [0, 0]]), points([[3, 4], [6, 4]])),
            5.0
        );
        assert!(
            (segment_distance(points([[0, 0], [8, 8]]), points([[0, 16], [8, 24]]))
                - 128.0_f64.sqrt())
            .abs()
                < f64::EPSILON
        );
    }
}
