use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use super::{
    TopologyBoundary, TopologyObservation,
    context::{
        PartitionContext, canonical, canonical_boundary, center,
        compare_boundaries as compare_boundary, distance, full_pair, point,
    },
};

const MAX_GROUP_SPANS: usize = 4;
const MIN_GROUP_SPANS: usize = 3;
const MAX_CHAIN_DISTANCE_PIXELS: f64 = 384.0;
const WIDTH_TOLERANCE_PIXELS: f64 = 8.0;
const GEOMETRY_TOLERANCE_PIXELS: f64 = 16.0;
const FLAT_DUPLICATE_DISTANCE_PIXELS: f64 = 128.0;
const FLAT_ENVELOPE_TOLERANCE_PIXELS: f64 = 32.0;
const PARALLEL_COSINE: f64 = 0.965_925_826_289_068_3; // cos(15 degrees)

#[derive(Clone, Copy)]
struct EdgeRecord {
    boundary: usize,
    left: u32,
    right: u32,
}

#[derive(Default)]
struct FullGraph {
    incidents: BTreeMap<u32, BTreeSet<usize>>,
    adjacency: BTreeMap<u32, Vec<usize>>,
    edges: Vec<EdgeRecord>,
}

#[derive(Clone)]
struct Link {
    region: u32,
    nodes: [usize; 2],
    span_unit: [f64; 2],
    span_normal: [f64; 2],
    flat_bounds: [[f64; 2]; 2],
    inside_flat_envelope: bool,
    centers: [[f64; 2]; 2],
    center_distance: f64,
    unit: [f64; 2],
    normal: [f64; 2],
    min_width: f64,
    longitudinal_min: f64,
    longitudinal_max: f64,
    transverse_min: f64,
    transverse_max: f64,
}

struct LinkGroup {
    links: Vec<usize>,
    nodes: BTreeSet<usize>,
}

pub(super) fn collapse_serial_boundaries(
    boundaries: &[TopologyBoundary],
    ctx: &PartitionContext<'_>,
) -> Vec<TopologyBoundary> {
    let mut result: Vec<_> = boundaries.iter().map(clone_boundary).collect();
    if ctx.partition.boundaries().len() != boundaries.len() {
        result.sort_by(compare_boundary);
        return result;
    }

    let graph = full_graph(ctx);
    let bridges = bridge_boundary_indices(&graph);
    let mut links = candidate_links(boundaries, ctx, &graph);
    let passing_links = measure_links(&mut links, ctx);
    let mut removed = BTreeSet::new();
    let mut replacements = BTreeMap::new();

    let serial_links: Vec<_> = links
        .iter()
        .enumerate()
        .filter_map(|(index, link)| {
            link.nodes
                .iter()
                .all(|node| bridges.contains(node))
                .then_some(index)
        })
        .collect();
    for group in connected_link_groups(&links, &serial_links) {
        if group.links.iter().any(|link| !passing_links.contains(link))
            || !group_is_simple_path(&group, &links, ctx, MIN_GROUP_SPANS)
        {
            continue;
        }
        let mut distances: Vec<_> = group
            .links
            .iter()
            .map(|link| links[*link].center_distance)
            .collect();
        distances.sort_by(f64::total_cmp);
        let total_distance: f64 = distances.into_iter().sum();
        let ends = group_end_nodes(&group, &links);
        let end_distance = distance(center(&boundaries[ends[0]]), center(&boundaries[ends[1]]));
        if total_distance > MAX_CHAIN_DISTANCE_PIXELS || end_distance > MAX_CHAIN_DISTANCE_PIXELS {
            continue;
        }
        let retained = representative(&group.nodes, boundaries);
        replacements.insert(
            retained,
            consolidated_boundary(retained, &group.nodes, boundaries),
        );
        removed.extend(group.nodes.into_iter().filter(|node| *node != retained));
    }

    // The context always has terrain cells, so this is the demo's terrain-flags path. It only
    // accepts an entirely flat, non-ramp component between close parallel spans.
    let flat_links: Vec<_> = links
        .iter()
        .enumerate()
        .filter_map(|(index, link)| {
            let stats = ctx.stats.get(&link.region)?;
            (link.inside_flat_envelope
                && !stats.has_ramp
                && stats.elevations.count_ones() == 1
                && link
                    .nodes
                    .iter()
                    .all(|node| !removed.contains(node) && !replacements.contains_key(node))
                && nearby_parallel_spans(&boundaries[link.nodes[0]], &boundaries[link.nodes[1]]))
            .then_some(index)
        })
        .collect();
    for group in connected_link_groups(&links, &flat_links) {
        if !group_is_simple_path(&group, &links, ctx, 2) {
            continue;
        }
        let nodes: Vec<_> = group.nodes.iter().copied().collect();
        if !nodes.iter().all(|left| {
            nodes
                .iter()
                .all(|right| nearby_parallel_spans(&boundaries[*left], &boundaries[*right]))
        }) {
            continue;
        }
        let retained = representative(&group.nodes, boundaries);
        replacements.insert(
            retained,
            consolidated_boundary(retained, &group.nodes, boundaries),
        );
        removed.extend(group.nodes.into_iter().filter(|node| *node != retained));
    }

    result = result
        .into_iter()
        .enumerate()
        .filter_map(|(index, boundary)| {
            (!removed.contains(&index)).then(|| replacements.remove(&index).unwrap_or(boundary))
        })
        .collect();
    result.sort_by(compare_boundary);
    result
}

fn full_graph(ctx: &PartitionContext<'_>) -> FullGraph {
    let mut graph = FullGraph::default();
    for (boundary, assessment) in ctx.partition.boundaries().iter().enumerate() {
        for &[left, right] in &assessment.region_pairs {
            graph.incidents.entry(left).or_default().insert(boundary);
            graph.incidents.entry(right).or_default().insert(boundary);
            let edge = graph.edges.len();
            graph.edges.push(EdgeRecord {
                boundary,
                left,
                right,
            });
            graph.adjacency.entry(left).or_default().push(edge);
            graph.adjacency.entry(right).or_default().push(edge);
        }
    }
    graph
}

/// Iterative Tarjan traversal. `parent_edge` deliberately identifies the edge record rather
/// than its boundary: a multigraph may have two different edges joining the same regions.
fn bridge_boundary_indices(graph: &FullGraph) -> BTreeSet<usize> {
    #[derive(Clone, Copy)]
    struct Frame {
        region: u32,
        parent_edge: Option<usize>,
        next: usize,
    }

    let mut discovery = BTreeMap::new();
    let mut low = BTreeMap::new();
    let mut bridges = BTreeSet::new();
    let mut time = 0_u32;
    for &root in graph.adjacency.keys() {
        if discovery.contains_key(&root) {
            continue;
        }
        time += 1;
        discovery.insert(root, time);
        low.insert(root, time);
        let mut stack = vec![Frame {
            region: root,
            parent_edge: None,
            next: 0,
        }];
        while let Some(frame) = stack.last_mut() {
            let adjacent = &graph.adjacency[&frame.region];
            if frame.next < adjacent.len() {
                let edge_index = adjacent[frame.next];
                frame.next += 1;
                if Some(edge_index) == frame.parent_edge {
                    continue;
                }
                let edge = graph.edges[edge_index];
                let other = if edge.left == frame.region {
                    edge.right
                } else {
                    edge.left
                };
                if let std::collections::btree_map::Entry::Vacant(entry) = discovery.entry(other) {
                    time += 1;
                    entry.insert(time);
                    low.insert(other, time);
                    stack.push(Frame {
                        region: other,
                        parent_edge: Some(edge_index),
                        next: 0,
                    });
                } else {
                    let current_low = low[&frame.region];
                    low.insert(frame.region, current_low.min(discovery[&other]));
                }
                continue;
            }
            let frame = stack.pop().expect("nonempty traversal stack");
            let Some(parent_edge) = frame.parent_edge else {
                continue;
            };
            let edge = graph.edges[parent_edge];
            let parent = if edge.left == frame.region {
                edge.right
            } else {
                edge.left
            };
            low.insert(parent, low[&parent].min(low[&frame.region]));
            if low[&frame.region] > discovery[&parent] {
                bridges.insert(edge.boundary);
            }
        }
    }
    bridges
}

fn candidate_links(
    boundaries: &[TopologyBoundary],
    ctx: &PartitionContext<'_>,
    graph: &FullGraph,
) -> Vec<Link> {
    let mut links = Vec::new();
    for (&region, incident) in &graph.incidents {
        if ctx.anchored.contains(&region) || incident.len() != 2 {
            continue;
        }
        let mut nodes: Vec<_> = incident.iter().copied().collect();
        nodes.sort_by(|left, right| compare_boundary(&boundaries[*left], &boundaries[*right]));
        if !nodes.iter().all(|index| {
            usable_span(&boundaries[*index])
                && boundaries[*index].ramp.is_none()
                && full_pair(&ctx.partition.boundaries()[*index]).is_some()
        }) {
            continue;
        }
        let pairs: Vec<_> = nodes
            .iter()
            .map(|index| full_pair(&ctx.partition.boundaries()[*index]).expect("validated above"))
            .collect();
        if !pairs
            .iter()
            .all(|pair| pair[0] == region || pair[1] == region)
        {
            continue;
        }
        let centers = [center(&boundaries[nodes[0]]), center(&boundaries[nodes[1]])];
        let delta = [centers[1][0] - centers[0][0], centers[1][1] - centers[0][1]];
        let center_distance = delta[0].hypot(delta[1]);
        if center_distance == 0.0 {
            continue;
        }
        let unit = [delta[0] / center_distance, delta[1] / center_distance];
        let endpoints = canonical(boundaries[nodes[0]].endpoints);
        let a = point(endpoints[0]);
        let b = point(endpoints[1]);
        let span_vector = [b[0] - a[0], b[1] - a[1]];
        let span_length = span_vector[0].hypot(span_vector[1]);
        let span_unit = [span_vector[0] / span_length, span_vector[1] / span_length];
        let span_normal = [-span_unit[1], span_unit[0]];
        let mut flat_bounds = [[f64::INFINITY, f64::NEG_INFINITY]; 2];
        for &index in &nodes {
            for endpoint in canonical(boundaries[index].endpoints) {
                let p = point(endpoint);
                for (axis, vector) in [span_unit, span_normal].iter().enumerate() {
                    let projection = p[0] * vector[0] + p[1] * vector[1];
                    flat_bounds[axis][0] = flat_bounds[axis][0].min(projection);
                    flat_bounds[axis][1] = flat_bounds[axis][1].max(projection);
                }
            }
        }
        links.push(Link {
            region,
            nodes: [nodes[0], nodes[1]],
            span_unit,
            span_normal,
            flat_bounds,
            inside_flat_envelope: true,
            centers,
            center_distance,
            unit,
            normal: [-unit[1], unit[0]],
            min_width: boundaries[nodes[0]]
                .width_pixels
                .min(boundaries[nodes[1]].width_pixels),
            longitudinal_min: f64::INFINITY,
            longitudinal_max: f64::NEG_INFINITY,
            transverse_min: f64::INFINITY,
            transverse_max: f64::NEG_INFINITY,
        });
    }
    links
}

fn measure_links(links: &mut [Link], ctx: &PartitionContext<'_>) -> BTreeSet<usize> {
    let by_region: BTreeMap<_, _> = links
        .iter()
        .enumerate()
        .map(|(index, link)| (link.region, index))
        .collect();
    let width = ctx.grid.width() as usize;
    for (index, &region) in ctx.partition.labels().iter().enumerate() {
        let Some(&link_index) = by_region.get(&region) else {
            continue;
        };
        let link = &mut links[link_index];
        let x = (index % width) as f64 * 8.0 + 4.0;
        let y = (index / width) as f64 * 8.0 + 4.0;
        if link.inside_flat_envelope {
            for (axis, vector) in [link.span_unit, link.span_normal].iter().enumerate() {
                let projection = x * vector[0] + y * vector[1];
                let radius = 4.0 * (vector[0].abs() + vector[1].abs());
                let tolerance = if axis == 0 {
                    FLAT_ENVELOPE_TOLERANCE_PIXELS.max(link.min_width / 4.0)
                } else {
                    FLAT_ENVELOPE_TOLERANCE_PIXELS
                };
                if projection - radius < link.flat_bounds[axis][0] - tolerance
                    || projection + radius > link.flat_bounds[axis][1] + tolerance
                {
                    link.inside_flat_envelope = false;
                }
            }
        }
        let relative = [x - link.centers[0][0], y - link.centers[0][1]];
        let longitudinal = relative[0] * link.unit[0] + relative[1] * link.unit[1];
        let transverse = relative[0] * link.normal[0] + relative[1] * link.normal[1];
        link.longitudinal_min = link.longitudinal_min.min(longitudinal);
        link.longitudinal_max = link.longitudinal_max.max(longitudinal);
        link.transverse_min = link.transverse_min.min(transverse);
        link.transverse_max = link.transverse_max.max(transverse);
    }
    links
        .iter()
        .enumerate()
        .filter_map(|(index, link)| {
            if !link.transverse_min.is_finite() {
                return None;
            }
            let cell_extent = 8.0 * (link.normal[0].abs() + link.normal[1].abs());
            let longitudinal_extent = 8.0 * (link.unit[0].abs() + link.unit[1].abs());
            let transverse_envelope = link.transverse_max - link.transverse_min + cell_extent;
            let longitudinal_envelope =
                link.longitudinal_max - link.longitudinal_min + longitudinal_extent;
            (transverse_envelope <= 1.5 * link.min_width + GEOMETRY_TOLERANCE_PIXELS
                && longitudinal_envelope
                    <= link.center_distance + link.min_width / 2.0 + GEOMETRY_TOLERANCE_PIXELS)
                .then_some(index)
        })
        .collect()
}

fn connected_link_groups(links: &[Link], selected: &[usize]) -> Vec<LinkGroup> {
    let mut by_node: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for &link in selected {
        for node in links[link].nodes {
            by_node.entry(node).or_default().push(link);
        }
    }
    let mut seen = BTreeSet::new();
    let mut groups = Vec::new();
    for &first in selected {
        if !seen.insert(first) {
            continue;
        }
        let mut queue = vec![first];
        let mut group_links = Vec::new();
        let mut nodes = BTreeSet::new();
        while let Some(link) = queue.pop() {
            group_links.push(link);
            for node in links[link].nodes {
                nodes.insert(node);
                for &adjacent in &by_node[&node] {
                    if seen.insert(adjacent) {
                        queue.push(adjacent);
                    }
                }
            }
        }
        groups.push(LinkGroup {
            links: group_links,
            nodes,
        });
    }
    groups
}

fn group_is_simple_path(
    group: &LinkGroup,
    links: &[Link],
    ctx: &PartitionContext<'_>,
    minimum_spans: usize,
) -> bool {
    if group.nodes.len() < minimum_spans
        || group.nodes.len() > MAX_GROUP_SPANS
        || group.links.len() != group.nodes.len() - 1
    {
        return false;
    }
    let mut degree: BTreeMap<_, _> = group.nodes.iter().map(|node| (*node, 0_usize)).collect();
    for link in &group.links {
        for node in &links[*link].nodes {
            *degree.get_mut(node).expect("group node") += 1;
        }
    }
    let ends: Vec<_> = degree
        .iter()
        .filter_map(|(&node, &degree)| (degree == 1).then_some(node))
        .collect();
    if ends.len() != 2 || degree.values().any(|degree| *degree == 0 || *degree > 2) {
        return false;
    }
    let exterior = |node: usize, region: u32| {
        let pair = full_pair(&ctx.partition.boundaries()[node]).expect("candidate link node");
        if pair[0] == region { pair[1] } else { pair[0] }
    };
    let outer: Vec<_> = ends
        .iter()
        .map(|&end| {
            let link = group
                .links
                .iter()
                .copied()
                .find(|link| links[*link].nodes.contains(&end))
                .expect("path end has a link");
            exterior(end, links[link].region)
        })
        .collect();
    outer[0] != outer[1]
}

fn group_end_nodes(group: &LinkGroup, links: &[Link]) -> Vec<usize> {
    let mut degree: BTreeMap<_, _> = group.nodes.iter().map(|node| (*node, 0_usize)).collect();
    for link in &group.links {
        for node in &links[*link].nodes {
            *degree.get_mut(node).expect("group node") += 1;
        }
    }
    degree
        .into_iter()
        .filter_map(|(node, degree)| (degree == 1).then_some(node))
        .collect()
}
fn representative(nodes: &BTreeSet<usize>, boundaries: &[TopologyBoundary]) -> usize {
    let mut members: Vec<_> = nodes.iter().copied().collect();
    members.sort_by(|left, right| compare_boundary(&boundaries[*left], &boundaries[*right]));
    let minimum_width = members
        .iter()
        .map(|index| boundaries[*index].width_pixels)
        .min_by(f64::total_cmp)
        .expect("nonempty group");
    let mut eligible: Vec<_> = members
        .iter()
        .copied()
        .filter(|index| boundaries[*index].width_pixels <= minimum_width + WIDTH_TOLERANCE_PIXELS)
        .collect();
    eligible.sort_by(|left, right| {
        let left_distance: f64 = members
            .iter()
            .map(|member| distance(center(&boundaries[*left]), center(&boundaries[*member])))
            .sum();
        let right_distance: f64 = members
            .iter()
            .map(|member| distance(center(&boundaries[*right]), center(&boundaries[*member])))
            .sum();
        left_distance
            .total_cmp(&right_distance)
            .then_with(|| compare_boundary(&boundaries[*left], &boundaries[*right]))
    });
    eligible[0]
}

fn consolidated_boundary(
    representative: usize,
    members: &BTreeSet<usize>,
    boundaries: &[TopologyBoundary],
) -> TopologyBoundary {
    let mut result = clone_boundary(&boundaries[representative]);
    let mut members: Vec<_> = members.iter().copied().collect();
    members.sort_by(|left, right| compare_boundary(&boundaries[*left], &boundaries[*right]));
    result.sources = sorted_sources(
        members
            .iter()
            .flat_map(|index| boundaries[*index].sources.iter().copied()),
    );
    result.observations = members
        .iter()
        .flat_map(|index| {
            let boundary = &boundaries[*index];
            if boundary.observations.is_empty() {
                vec![TopologyObservation {
                    endpoints: boundary.endpoints,
                    width_pixels: boundary.width_pixels,
                    sources: boundary.sources.clone(),
                }]
            } else {
                boundary.observations.clone()
            }
        })
        .map(clone_observation)
        .collect();
    result.observations.sort_by(compare_observation);
    result
}

fn nearby_parallel_spans(left: &TopologyBoundary, right: &TopologyBoundary) -> bool {
    let a = canonical(left.endpoints);
    let mut b = canonical(right.endpoints);
    let av = [a[1].x as f64 - a[0].x as f64, a[1].y as f64 - a[0].y as f64];
    let bv = [b[1].x as f64 - b[0].x as f64, b[1].y as f64 - b[0].y as f64];
    let dot = av[0] * bv[0] + av[1] * bv[1];
    if dot.abs() / av[0].hypot(av[1]) / bv[0].hypot(bv[1]) < PARALLEL_COSINE {
        return false;
    }
    if dot < 0.0 {
        b.swap(0, 1);
    }
    a.into_iter()
        .zip(b)
        .all(|(left, right)| distance(point(left), point(right)) <= FLAT_DUPLICATE_DISTANCE_PIXELS)
}

fn usable_span(boundary: &TopologyBoundary) -> bool {
    boundary.endpoints[0] != boundary.endpoints[1]
}

fn clone_boundary(boundary: &TopologyBoundary) -> TopologyBoundary {
    let mut result = canonical_boundary(boundary.clone());
    result.sources = sorted_sources(result.sources);
    result.observations = result
        .observations
        .into_iter()
        .map(clone_observation)
        .collect();
    result.observations.sort_by(compare_observation);
    result
}

fn clone_observation(mut observation: TopologyObservation) -> TopologyObservation {
    observation.endpoints = canonical(observation.endpoints);
    observation.sources = sorted_sources(observation.sources);
    observation
}

fn sorted_sources(sources: impl IntoIterator<Item = [u32; 2]>) -> Vec<[u32; 2]> {
    let mut result: Vec<_> = sources.into_iter().collect();
    result.sort_unstable();
    result.dedup();
    result
}

fn compare_observation(left: &TopologyObservation, right: &TopologyObservation) -> Ordering {
    canonical(left.endpoints)
        .cmp(&canonical(right.endpoints))
        .then_with(|| left.width_pixels.total_cmp(&right.width_pixels))
        .then_with(|| left.sources.cmp(&right.sources))
}

#[cfg(test)]
mod tests {
    use super::{EdgeRecord, FullGraph, bridge_boundary_indices, collapse_serial_boundaries};
    use crate::{
        BoundarySpan, PixelPosition, TerrainCell, TerrainGrid, WalkPosition,
        topology::{
            RampBoundary, RampSide, TopologyBase, TopologyBoundary, TopologyObservation,
            context::PartitionContext,
        },
    };

    fn grid(width: u32, height: u32) -> TerrainGrid {
        TerrainGrid::from_cells(
            width,
            height,
            vec![
                TerrainCell {
                    walkable: true,
                    terrain_buildable: true,
                    elevation: 0,
                    ramp: false,
                };
                (width * height) as usize
            ],
        )
        .unwrap()
    }

    fn boundary(
        x: u32,
        sources: Vec<[u32; 2]>,
        observations: Vec<TopologyObservation>,
    ) -> TopologyBoundary {
        TopologyBoundary {
            endpoints: [PixelPosition { x, y: 32 }, PixelPosition { x, y: 0 }],
            width_pixels: 32.0,
            sources,
            observations,
            ramp: None,
        }
    }

    fn vertical_boundary(x: u32, height: u32) -> TopologyBoundary {
        TopologyBoundary {
            endpoints: [PixelPosition { x, y: height }, PixelPosition { x, y: 0 }],
            width_pixels: 32.0,
            sources: vec![],
            observations: vec![],
            ramp: None,
        }
    }

    fn serial_spans(xs: &[u32], height: u32) -> Vec<BoundarySpan> {
        xs.iter()
            .map(|&x| BoundarySpan {
                endpoints: [PixelPosition { x, y: 0 }, PixelPosition { x, y: height }],
            })
            .collect()
    }
    #[test]
    fn collapses_a_real_three_cut_serial_partition_and_retains_evidence() {
        let terrain = grid(28, 4);
        let spans = [
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 48, y: 0 },
                    PixelPosition { x: 48, y: 32 },
                ],
            },
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 96, y: 0 },
                    PixelPosition { x: 96, y: 32 },
                ],
            },
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 144, y: 0 },
                    PixelPosition { x: 144, y: 32 },
                ],
            },
        ];
        let partition = terrain.partition_by_spans(&spans).unwrap();
        assert_eq!(partition.areas().len(), 4);
        let boundaries = vec![
            boundary(48, vec![[3, 2], [1, 9]], vec![]),
            boundary(
                96,
                vec![[2, 3]],
                vec![TopologyObservation {
                    endpoints: [
                        PixelPosition { x: 96, y: 32 },
                        PixelPosition { x: 96, y: 0 },
                    ],
                    width_pixels: 32.0,
                    sources: vec![[7, 1]],
                }],
            ),
            boundary(144, vec![[1, 9], [5, 4]], vec![]),
        ];
        let context = PartitionContext::new(&terrain, &partition, &[]);

        let actual = collapse_serial_boundaries(&boundaries, &context);

        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual[0].endpoints,
            [
                PixelPosition { x: 96, y: 0 },
                PixelPosition { x: 96, y: 32 }
            ]
        );
        assert_eq!(actual[0].sources, vec![[1, 9], [2, 3], [3, 2], [5, 4]]);
        assert_eq!(actual[0].observations.len(), 3);
        assert_eq!(
            actual[0].observations[0].endpoints,
            [
                PixelPosition { x: 48, y: 0 },
                PixelPosition { x: 48, y: 32 }
            ]
        );
        assert_eq!(actual[0].observations[1].sources, vec![[7, 1]]);
        assert_eq!(
            actual[0].observations[2].endpoints,
            [
                PixelPosition { x: 144, y: 0 },
                PixelPosition { x: 144, y: 32 }
            ]
        );
    }

    #[test]
    fn collapses_close_parallel_cuts_enclosing_a_flat_strip() {
        let terrain = grid(20, 4);
        let spans = [
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 64, y: 0 },
                    PixelPosition { x: 64, y: 32 },
                ],
            },
            BoundarySpan {
                endpoints: [
                    PixelPosition { x: 96, y: 0 },
                    PixelPosition { x: 96, y: 32 },
                ],
            },
        ];
        let partition = terrain.partition_by_spans(&spans).unwrap();
        assert_eq!(partition.areas().len(), 3);
        let boundaries = vec![
            boundary(64, vec![[8, 2]], vec![]),
            boundary(96, vec![[3, 7]], vec![]),
        ];
        let context = PartitionContext::new(&terrain, &partition, &[]);

        let actual = collapse_serial_boundaries(&boundaries, &context);

        assert_eq!(actual.len(), 1);
        assert_eq!(
            actual[0].endpoints,
            [
                PixelPosition { x: 64, y: 0 },
                PixelPosition { x: 64, y: 32 }
            ]
        );
        assert_eq!(actual[0].sources, vec![[3, 7], [8, 2]]);
        assert_eq!(actual[0].observations.len(), 2);
    }
    #[test]
    fn preserves_serial_boundaries_when_intermediate_regions_are_anchored() {
        let terrain = grid(28, 4);
        let spans = serial_spans(&[48, 96, 144], 32);
        let partition = terrain.partition_by_spans(&spans).unwrap();
        let boundaries = spans
            .iter()
            .map(|span| vertical_boundary(span.endpoints[0].x, 32))
            .collect::<Vec<_>>();
        let bases = [
            TopologyBase {
                id: 7,
                route_anchor: Some(WalkPosition { x: 8, y: 1 }),
            },
            TopologyBase {
                id: 8,
                route_anchor: Some(WalkPosition { x: 14, y: 1 }),
            },
        ];
        let context = PartitionContext::new(&terrain, &partition, &bases);

        assert_eq!(collapse_serial_boundaries(&boundaries, &context).len(), 3);
    }

    #[test]
    fn preserves_serial_boundaries_when_a_member_is_a_ramp() {
        let terrain = grid(28, 4);
        let spans = serial_spans(&[48, 96, 144], 32);
        let partition = terrain.partition_by_spans(&spans).unwrap();
        let mut boundaries = spans
            .iter()
            .map(|span| vertical_boundary(span.endpoints[0].x, 32))
            .collect::<Vec<_>>();
        boundaries[1].ramp = Some(RampBoundary {
            id: 3,
            end: RampSide::Lower,
            lower_elevation: 0,
            upper_elevation: 1,
        });
        let context = PartitionContext::new(&terrain, &partition, &[]);

        assert_eq!(collapse_serial_boundaries(&boundaries, &context).len(), 3);
    }

    #[test]
    fn bridge_walk_distinguishes_parallel_edges_and_cycles() {
        let mut graph = FullGraph {
            edges: vec![
                EdgeRecord {
                    boundary: 10,
                    left: 1,
                    right: 2,
                },
                EdgeRecord {
                    boundary: 11,
                    left: 1,
                    right: 2,
                },
                EdgeRecord {
                    boundary: 12,
                    left: 2,
                    right: 3,
                },
                EdgeRecord {
                    boundary: 13,
                    left: 3,
                    right: 1,
                },
                EdgeRecord {
                    boundary: 14,
                    left: 3,
                    right: 4,
                },
            ],
            ..Default::default()
        };
        for (edge, record) in graph.edges.iter().enumerate() {
            graph.adjacency.entry(record.left).or_default().push(edge);
            graph.adjacency.entry(record.right).or_default().push(edge);
        }

        assert_eq!(
            bridge_boundary_indices(&graph)
                .into_iter()
                .collect::<Vec<_>>(),
            vec![14]
        );
    }

    #[test]
    fn rejects_long_serial_corridors_and_regions_wider_than_their_spans() {
        let long_terrain = grid(60, 4);
        let long_spans = serial_spans(&[32, 168, 304, 440], 32);
        let long_partition = long_terrain.partition_by_spans(&long_spans).unwrap();
        let long_boundaries = long_spans
            .iter()
            .map(|span| vertical_boundary(span.endpoints[0].x, 32))
            .collect::<Vec<_>>();
        let long_context = PartitionContext::new(&long_terrain, &long_partition, &[]);
        assert_eq!(
            collapse_serial_boundaries(&long_boundaries, &long_context).len(),
            long_boundaries.len()
        );

        let wide_terrain = grid(50, 12);
        let wide_spans = serial_spans(&[48, 200, 352], 96);
        let wide_partition = wide_terrain.partition_by_spans(&wide_spans).unwrap();
        let wide_boundaries = wide_spans
            .iter()
            .map(|span| vertical_boundary(span.endpoints[0].x, 96))
            .collect::<Vec<_>>();
        let wide_context = PartitionContext::new(&wide_terrain, &wide_partition, &[]);
        assert_eq!(
            collapse_serial_boundaries(&wide_boundaries, &wide_context).len(),
            wide_boundaries.len()
        );
    }

    #[test]
    fn flat_strip_with_a_ramp_cell_is_not_collapsed() {
        let mut cells = vec![
            TerrainCell {
                walkable: true,
                terrain_buildable: true,
                elevation: 0,
                ramp: false,
            };
            20 * 4
        ];
        cells[20 + 10] = TerrainCell {
            ramp: true,
            ..cells[20 + 10]
        };
        let terrain = TerrainGrid::from_cells(20, 4, cells).unwrap();
        let spans = serial_spans(&[64, 96], 32);
        let partition = terrain.partition_by_spans(&spans).unwrap();
        let boundaries = spans
            .iter()
            .map(|span| vertical_boundary(span.endpoints[0].x, 32))
            .collect::<Vec<_>>();
        let context = PartitionContext::new(&terrain, &partition, &[]);

        assert_eq!(collapse_serial_boundaries(&boundaries, &context).len(), 2);
    }

    #[test]
    fn output_is_input_order_independent_and_keeps_sorted_source_evidence() {
        let terrain = grid(28, 4);
        let spans = serial_spans(&[48, 96, 144], 32);
        let partition = terrain.partition_by_spans(&spans).unwrap();
        let boundaries = vec![
            boundary(48, vec![[9, 1], [3, 2]], vec![]),
            boundary(96, vec![[2, 3]], vec![]),
            boundary(144, vec![[1, 9], [3, 2]], vec![]),
        ];
        let context = PartitionContext::new(&terrain, &partition, &[]);
        let expected = collapse_serial_boundaries(&boundaries, &context);

        let order = [2, 0, 1];
        let shuffled_spans = order.map(|index| spans[index]);
        let shuffled_partition = terrain.partition_by_spans(&shuffled_spans).unwrap();
        let shuffled_boundaries = order
            .map(|index| {
                let mut boundary = boundaries[index].clone();
                boundary.endpoints.reverse();
                boundary
            })
            .to_vec();
        let shuffled_context = PartitionContext::new(&terrain, &shuffled_partition, &[]);
        let actual = collapse_serial_boundaries(&shuffled_boundaries, &shuffled_context);

        assert_eq!(actual, expected);
        assert_eq!(actual[0].sources, vec![[1, 9], [2, 3], [3, 2], [9, 1]]);
        assert_eq!(actual[0].observations.len(), 3);
    }
}
