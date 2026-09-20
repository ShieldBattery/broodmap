use std::collections::{BTreeMap, BTreeSet};

use super::TopologyBoundary;
use super::context::{
    PartitionContext, RegionStats, canonical_output, center, compare_boundaries, distance,
    full_pair, point, point_segment_distance,
};

const MAX_PORT_DISTANCE_PIXELS: f64 = 384.0;
const MAX_UNION_RADIUS_PIXELS: f64 = 512.0;

#[derive(Clone)]
struct UnionProposal {
    index: usize,
    pair: [u32; 2],
    anchored_regions: usize,
    ports: BTreeSet<usize>,
}
#[derive(Clone)]
struct BridgeProposal {
    index: usize,
    pocket: u32,
    exterior: u32,
    ports: BTreeSet<usize>,
}
#[derive(Clone)]
enum Proposal {
    Union(UnionProposal),
    Bridge(BridgeProposal),
}
impl Proposal {
    fn index(&self) -> usize {
        match self {
            Self::Union(x) => x.index,
            Self::Bridge(x) => x.index,
        }
    }
    fn tie_key(&self) -> (u8, u32, u32) {
        match self {
            Self::Union(x) => (0, x.pair[0].min(x.pair[1]), x.pair[0].max(x.pair[1])),
            Self::Bridge(x) => (1, x.pocket, x.exterior),
        }
    }
    fn ports(&self) -> &BTreeSet<usize> {
        match self {
            Self::Union(x) => &x.ports,
            Self::Bridge(x) => &x.ports,
        }
    }
}

fn within_distance(left: [f64; 2], right: [f64; 2], limit: f64) -> bool {
    distance(left, right) <= limit
}
fn within_radius(stats: &RegionStats, middle: [f64; 2]) -> bool {
    stats.cells > 0
        && stats
            .corners()
            .into_iter()
            .all(|corner| within_distance(corner, middle, MAX_UNION_RADIUS_PIXELS))
}
fn within_span_radius(stats: &RegionStats, boundary: &TopologyBoundary) -> bool {
    let [start, end] = boundary.endpoints;
    stats.cells > 0
        && stats.corners().into_iter().all(|corner| {
            point_segment_distance(corner, point(start), point(end)) <= MAX_UNION_RADIUS_PIXELS
        })
}
fn dominant_elevation(stats: &RegionStats) -> Option<usize> {
    let dominant = (1..4).fold(0, |best, elevation| {
        if stats.elevation_counts[elevation] > stats.elevation_counts[best] {
            elevation
        } else {
            best
        }
    });
    (stats.elevation_counts[dominant] * 100 >= stats.cells * 99).then_some(dominant)
}
fn flat(stats: &RegionStats) -> bool {
    stats.elevations != 0 && stats.elevations.count_ones() == 1
}
/// Removes local redundant junction dividers while retaining all exterior ports and provenance.
pub(super) fn remove_junction_boundaries(
    boundaries: &[TopologyBoundary],
    ctx: &PartitionContext<'_>,
) -> Vec<TopologyBoundary> {
    if boundaries.len() != ctx.partition.boundaries().len() {
        return canonical_output(boundaries, &BTreeSet::new());
    }
    let metadata = ctx.partition.boundaries();
    let mut union_proposals = Vec::new();
    for (index, entry) in metadata.iter().enumerate() {
        let Some(pair) = full_pair(entry) else {
            continue;
        };
        if boundaries[index].ramp.is_some() {
            continue;
        }
        let anchored_regions = pair
            .into_iter()
            .filter(|region| ctx.anchored.contains(region))
            .count();
        if anchored_regions > 1 {
            continue;
        }
        let union = BTreeSet::from(pair);
        let mut ports = BTreeSet::new();
        let mut valid = true;
        for region in pair {
            for other_index in ctx.incidents.get(&region).into_iter().flatten() {
                if *other_index == index {
                    continue;
                }
                let Some(other_pair) = full_pair(&metadata[*other_index]) else {
                    valid = false;
                    break;
                };
                if union.contains(&other_pair[0]) == union.contains(&other_pair[1]) {
                    valid = false;
                    break;
                }
                ports.insert(*other_index);
            }
            if !valid {
                break;
            }
        }
        if !valid || ports.len() < 3 {
            continue;
        }
        let mut exteriors = BTreeSet::new();
        let mut widest_port: f64 = 0.0;
        let candidate_center = center(&boundaries[index]);
        for port in &ports {
            let other_pair = full_pair(&metadata[*port]).expect("validated port");
            exteriors.insert(if union.contains(&other_pair[0]) {
                other_pair[1]
            } else {
                other_pair[0]
            });
            widest_port = widest_port.max(boundaries[*port].width_pixels);
            if !within_distance(
                candidate_center,
                center(&boundaries[*port]),
                MAX_PORT_DISTANCE_PIXELS,
            ) {
                valid = false;
                break;
            }
        }
        if valid
            && exteriors.len() >= 3
            && (anchored_regions != 1 || boundaries[index].width_pixels >= widest_port)
        {
            union_proposals.push(UnionProposal {
                index,
                pair,
                anchored_regions,
                ports,
            });
        }
    }

    let mut bridge_proposals = Vec::new();
    for (pocket, ports) in &ctx.incidents {
        if ctx.anchored.contains(pocket) || ports.len() != 3 {
            continue;
        }
        if ports.iter().any(|index| {
            boundaries[*index].ramp.is_some() || full_pair(&metadata[*index]).is_none()
        }) {
            continue;
        }
        let mut groups: BTreeMap<u32, Vec<usize>> = BTreeMap::new();
        for index in ports {
            let pair = full_pair(&metadata[*index]).expect("validated bridge port");
            let Some(exterior) = (pair[0] == *pocket)
                .then_some(pair[1])
                .or_else(|| (pair[1] == *pocket).then_some(pair[0]))
            else {
                continue;
            };
            groups.entry(exterior).or_default().push(*index);
        }
        if groups.len() != 2 {
            continue;
        }
        let Some((&exterior, shared)) = groups.iter().find(|(_, indexes)| indexes.len() == 2)
        else {
            continue;
        };
        let Some((&third, _)) = groups.iter().find(|(_, indexes)| indexes.len() == 1) else {
            continue;
        };
        if !ctx.anchored.contains(&third) {
            continue;
        }
        let (first, second) = (shared[0], shared[1]);
        let index = if boundaries[first].width_pixels >= 2.0 * boundaries[second].width_pixels {
            first
        } else if boundaries[second].width_pixels >= 2.0 * boundaries[first].width_pixels {
            second
        } else {
            continue;
        };
        let middle = center(&boundaries[index]);
        if ports.iter().all(|port| {
            within_distance(middle, center(&boundaries[*port]), MAX_PORT_DISTANCE_PIXELS)
        }) {
            bridge_proposals.push(BridgeProposal {
                index,
                pocket: *pocket,
                exterior,
                ports: ports.clone(),
            });
        }
    }

    let mut ranked = Vec::new();
    for proposal in union_proposals {
        let (Some(first), Some(second)) = (
            ctx.stats.get(&proposal.pair[0]),
            ctx.stats.get(&proposal.pair[1]),
        ) else {
            continue;
        };
        let local = if proposal.anchored_regions == 1 {
            within_span_radius(first, &boundaries[proposal.index])
                && within_span_radius(second, &boundaries[proposal.index])
        } else {
            within_radius(first, center(&boundaries[proposal.index]))
                && within_radius(second, center(&boundaries[proposal.index]))
        };
        let strictly_flat = (first.elevations | second.elevations).count_ones() == 1;
        let near_flat = proposal.anchored_regions == 1
            && dominant_elevation(first)
                .zip(dominant_elevation(second))
                .is_some_and(|(a, b)| a == b);
        if first.cells + second.cells > 0
            && !first.has_ramp
            && !second.has_ramp
            && local
            && (strictly_flat || near_flat)
        {
            ranked.push(Proposal::Union(proposal));
        }
    }
    for proposal in bridge_proposals {
        let Some(stats) = ctx.stats.get(&proposal.pocket) else {
            continue;
        };
        if !stats.has_ramp
            && within_radius(stats, center(&boundaries[proposal.index]))
            && flat(stats)
        {
            ranked.push(Proposal::Bridge(proposal));
        }
    }
    let removed = select_proposals(boundaries, ranked);
    canonical_output(boundaries, &removed)
}

fn select_proposals(boundaries: &[TopologyBoundary], mut ranked: Vec<Proposal>) -> BTreeSet<usize> {
    ranked.sort_by(|left, right| {
        boundaries[right.index()]
            .width_pixels
            .total_cmp(&boundaries[left.index()].width_pixels)
            .then_with(|| compare_boundaries(&boundaries[left.index()], &boundaries[right.index()]))
            // Equal-cut bridge proposals reserve different ports. Use region identities rather
            // than discovery order, with unions first as in the original two-pass generation.
            .then_with(|| left.tie_key().cmp(&right.tie_key()))
    });

    let mut removed = BTreeSet::new();
    let mut reserved_ports = BTreeSet::new();
    let mut reserved_unions = BTreeSet::new();
    let mut reserved_bridge_pockets = BTreeSet::new();
    let mut reserved_bridge_exteriors = BTreeSet::new();
    for proposal in ranked {
        if proposal
            .ports()
            .iter()
            .any(|port| removed.contains(port) || reserved_ports.contains(port))
        {
            continue;
        }
        match &proposal {
            Proposal::Union(value) => {
                if reserved_ports.contains(&value.index)
                    || value.pair.into_iter().any(|region| {
                        reserved_unions.contains(&region)
                            || reserved_bridge_pockets.contains(&region)
                            || reserved_bridge_exteriors.contains(&region)
                    })
                {
                    continue;
                }
                removed.insert(value.index);
                reserved_unions.extend(value.pair);
            }
            Proposal::Bridge(value) => {
                if reserved_ports.contains(&value.index)
                    || reserved_unions.contains(&value.pocket)
                    || reserved_unions.contains(&value.exterior)
                    || reserved_bridge_pockets.contains(&value.pocket)
                    || reserved_bridge_pockets.contains(&value.exterior)
                    || reserved_bridge_exteriors.contains(&value.pocket)
                {
                    continue;
                }
                removed.insert(value.index);
                reserved_bridge_pockets.insert(value.pocket);
                reserved_bridge_exteriors.insert(value.exterior);
            }
        }
        reserved_ports.extend(proposal.ports().iter().copied());
    }
    removed
}

#[cfg(test)]
mod tests {
    use super::super::TopologyBase;
    use super::super::context::canonical;
    use super::*;
    use crate::{BoundarySpan, PixelPosition, TerrainCell, TerrainGrid, WalkPosition};
    fn span(ax: u32, ay: u32, bx: u32, by: u32) -> BoundarySpan {
        BoundarySpan {
            endpoints: [
                PixelPosition { x: ax, y: ay },
                PixelPosition { x: bx, y: by },
            ],
        }
    }
    fn grid() -> TerrainGrid {
        TerrainGrid::from_cells(
            64,
            64,
            (0..64)
                .flat_map(|y| {
                    (0..64).map(move |x| {
                        let room = |l, t, r, b| (l..=r).contains(&x) && (t..=b).contains(&y);
                        let walkable = room(30, 30, 38, 38)
                            || room(42, 30, 50, 38)
                            || room(12, 30, 20, 38)
                            || room(30, 12, 38, 20)
                            || room(42, 50, 50, 58)
                            || ((21..=29).contains(&x) && y == 32)
                            || (x == 32 && (21..=29).contains(&y))
                            || ((39..=41).contains(&x) && y == 34)
                            || (x == 46 && (39..=49).contains(&y));
                        TerrainCell {
                            walkable,
                            terrain_buildable: true,
                            elevation: 0,
                            ramp: false,
                        }
                    })
                })
                .collect(),
        )
        .unwrap()
    }
    fn boundaries() -> Vec<TopologyBoundary> {
        [
            span(200, 248, 200, 272),
            span(248, 200, 272, 200),
            span(320, 264, 320, 288),
            span(356, 312, 380, 312),
        ]
        .into_iter()
        .zip([120.0, 120.0, 400.0, 120.0])
        .map(|(span, width_pixels)| TopologyBoundary {
            endpoints: span.endpoints,
            width_pixels,
            sources: vec![[1, 2]],
            observations: Vec::new(),
            ramp: None,
        })
        .collect()
    }
    fn partition(grid: &TerrainGrid, boundaries: &[TopologyBoundary]) -> crate::SpanPartition {
        let spans = boundaries
            .iter()
            .map(|boundary| BoundarySpan {
                endpoints: boundary.endpoints,
            })
            .collect::<Vec<_>>();
        grid.partition_by_spans(&spans).unwrap()
    }
    #[test]
    fn removes_only_wide_anchored_junction_divider_from_real_partition() {
        let terrain = grid();
        let boundaries = boundaries();
        let partition = partition(&terrain, &boundaries);
        let ctx = PartitionContext::new(
            &terrain,
            &partition,
            &[TopologyBase {
                id: 7,
                route_anchor: Some(WalkPosition { x: 34, y: 34 }),
            }],
        );
        let result = remove_junction_boundaries(&boundaries, &ctx);
        assert_eq!(result.len(), 3);
        assert!(
            !result
                .iter()
                .any(|boundary| boundary.endpoints == canonical(boundaries[2].endpoints))
        );
    }
    #[test]
    fn retains_real_divider_when_both_sides_are_anchored() {
        let terrain = grid();
        let boundaries = boundaries();
        let partition = partition(&terrain, &boundaries);
        let ctx = PartitionContext::new(
            &terrain,
            &partition,
            &[
                TopologyBase {
                    id: 7,
                    route_anchor: Some(WalkPosition { x: 34, y: 34 }),
                },
                TopologyBase {
                    id: 8,
                    route_anchor: Some(WalkPosition { x: 46, y: 34 }),
                },
            ],
        );
        assert_eq!(
            remove_junction_boundaries(&boundaries, &ctx).len(),
            boundaries.len()
        );
    }

    #[test]
    fn anchored_union_rejects_narrow_necks_ramps_and_mismatched_elevation() {
        for variant in 0..4 {
            let original = grid();
            let mut cells = original.cells().to_vec();
            if variant == 1 {
                cells[34 * 64 + 34].ramp = true;
            }
            if variant == 2 {
                for y in 30..=38 {
                    for x in 42..=50 {
                        cells[y * 64 + x].elevation = 1;
                    }
                }
            }
            let terrain = TerrainGrid::from_cells(64, 64, cells).unwrap();
            let mut boundaries = boundaries();
            if variant == 0 {
                boundaries[2].width_pixels = 119.0;
            }
            if variant == 3 {
                boundaries[2].ramp = Some(super::super::RampBoundary {
                    id: 1,
                    end: super::super::RampSide::Lower,
                    lower_elevation: 0,
                    upper_elevation: 1,
                });
            }
            let partition = partition(&terrain, &boundaries);
            let ctx = PartitionContext::new(
                &terrain,
                &partition,
                &[TopologyBase {
                    id: 7,
                    route_anchor: Some(WalkPosition { x: 34, y: 34 }),
                }],
            );
            assert_eq!(
                remove_junction_boundaries(&boundaries, &ctx).len(),
                4,
                "variant {variant}"
            );
        }
    }
    #[test]
    fn near_flat_contact_requires_an_anchor_and_ninety_nine_percent_dominance() {
        for changed in [1, 2] {
            let original = grid();
            let mut cells = original.cells().to_vec();
            // Enlarge this real room to make one contact cell <1%, but two >1%.
            for y in 39..=42 {
                for x in 30..=38 {
                    cells[y * 64 + x].walkable = true;
                }
            }
            for x in 0..changed {
                cells[34 * 64 + 34 + x].elevation = 1;
            }
            let terrain = TerrainGrid::from_cells(64, 64, cells).unwrap();
            let boundaries = boundaries();
            let partition = partition(&terrain, &boundaries);
            let ctx = PartitionContext::new(
                &terrain,
                &partition,
                &[TopologyBase {
                    id: 7,
                    route_anchor: Some(WalkPosition { x: 34, y: 34 }),
                }],
            );
            assert_eq!(
                remove_junction_boundaries(&boundaries, &ctx).len(),
                if changed == 1 { 3 } else { 4 }
            );
            let unanchored = PartitionContext::new(&terrain, &partition, &[]);
            assert_eq!(
                remove_junction_boundaries(&boundaries, &unanchored).len(),
                4
            );
        }
    }
    #[test]
    fn junction_output_is_invariant_to_cut_order_and_endpoint_direction() {
        let terrain = grid();
        let boundaries = boundaries();
        let snapshot = partition(&terrain, &boundaries);
        let expected = remove_junction_boundaries(
            &boundaries,
            &PartitionContext::new(&terrain, &snapshot, &[]),
        );
        let mut reordered = boundaries.clone();
        reordered.reverse();
        for b in &mut reordered {
            b.endpoints.reverse();
        }
        let snapshot = partition(&terrain, &reordered);
        let actual = remove_junction_boundaries(
            &reordered,
            &PartitionContext::new(&terrain, &snapshot, &[]),
        );
        assert_eq!(actual, expected);
        assert_eq!(actual.len(), 3);
    }
    #[test]
    fn equal_cut_pockets_choose_lowest_id_before_reserving_later_ports() {
        let mut cuts = boundaries();
        cuts[0].width_pixels = 500.0;
        cuts[3].width_pixels = 200.0;
        // Supply every referenced port; this test isolates ranking/reservation from discovery.
        while cuts.len() < 7 {
            let mut port = cuts[1].clone();
            for endpoint in &mut port.endpoints {
                endpoint.x += cuts.len() as u32 * 8;
            }
            cuts.push(port);
        }
        let low = Proposal::Bridge(BridgeProposal {
            index: 0,
            pocket: 3,
            exterior: 7,
            ports: BTreeSet::from([0, 1, 2]),
        });
        let high = Proposal::Bridge(BridgeProposal {
            index: 0,
            pocket: 7,
            exterior: 3,
            ports: BTreeSet::from([0, 1, 4]),
        });
        let later = Proposal::Union(UnionProposal {
            index: 3,
            pair: [10, 11],
            anchored_regions: 0,
            ports: BTreeSet::from([2, 5, 6]),
        });
        // Choosing pocket 3 reserves port 2 and protects this later union's exterior exit.
        for proposals in [
            vec![low.clone(), high.clone(), later.clone()],
            vec![later.clone(), high.clone(), low.clone()],
        ] {
            assert_eq!(select_proposals(&cuts, proposals), BTreeSet::from([0]));
        }
        // The opposite pocket really would permit the later removal, so the tie is observable.
        assert_eq!(
            select_proposals(&cuts, vec![high, later]),
            BTreeSet::from([0, 3])
        );
    }

    #[test]
    fn bridge_pocket_retains_narrow_exit_and_obeys_anchor_geometry_and_terrain_gates() {
        for variant in 0..7 {
            let terrain = TerrainGrid::from_cells(
                100,
                140,
                (0..140)
                    .flat_map(|y| {
                        (0..100).map(move |x| {
                            let room = |l, t, r, b| (l..=r).contains(&x) && (t..=b).contains(&y);
                            TerrainCell {
                                walkable: room(10, 30, 25, 50)
                                    || room(40, 30, 60, 50)
                                    || room(70, 35, 85, 45)
                                    || ((26..=39).contains(&x) && (y == 34 || y == 46))
                                    || ((61..=69).contains(&x) && y == 40)
                                    || (variant == 6 && x == 50 && (51..=130).contains(&y)),
                                terrain_buildable: true,
                                elevation: u8::from(variant == 5 && x == 50 && y == 40),
                                ramp: variant == 4 && x == 50 && y == 40,
                            }
                        })
                    })
                    .collect(),
            )
            .unwrap();
            let boundaries: Vec<_> = [
                span(272, 264, 272, 288),
                span(272, 360, 272, 384),
                span(536, 312, 536, 336),
            ]
            .into_iter()
            .zip([350.0, if variant == 3 { 180.0 } else { 90.0 }, 280.0])
            .map(|(span, width_pixels)| TopologyBoundary {
                endpoints: span.endpoints,
                width_pixels,
                sources: vec![[1, 2]],
                observations: vec![],
                ramp: None,
            })
            .collect();
            let mut bases = vec![TopologyBase {
                id: 1,
                route_anchor: Some(WalkPosition { x: 15, y: 40 }),
            }];
            if variant != 2 {
                bases.push(TopologyBase {
                    id: 2,
                    route_anchor: Some(WalkPosition { x: 75, y: 40 }),
                });
            }
            if variant == 1 {
                bases.push(TopologyBase {
                    id: 3,
                    route_anchor: Some(WalkPosition { x: 50, y: 40 }),
                });
            }
            let snapshot = partition(&terrain, &boundaries);
            let ctx = PartitionContext::new(&terrain, &snapshot, &bases);
            let actual = remove_junction_boundaries(&boundaries, &ctx);
            if variant == 0 {
                assert_eq!(actual, vec![boundaries[1].clone(), boundaries[2].clone()]);
            } else {
                assert_eq!(actual.len(), 3, "variant {variant}");
            }
        }
    }
}
