use std::collections::{HashSet, VecDeque};

use broodmap_analysis::{BoundarySpan, PixelPosition, TerrainCell, TerrainGrid, WalkPosition};

const PIXELS_PER_CELL: i64 = 8;

#[derive(Debug, Copy, Clone)]
struct Edge {
    left: usize,
    right: usize,
    left_position: WalkPosition,
    right_position: WalkPosition,
}

#[derive(Debug, PartialEq, Eq)]
struct OraclePartition {
    labels: Vec<u32>,
    areas: Vec<u32>,
    boundaries: Vec<OracleBoundary>,
}

#[derive(Debug, PartialEq, Eq)]
struct OracleBoundary {
    removed_edge_count: u32,
    separated_edge_count: u32,
    region_pairs: Vec<[u32; 2]>,
}

#[test]
fn exhaustive_three_by_three_masks_match_independent_finite_span_oracle() {
    // These cover cardinal, diagonal, oblique, center-line, endpoint, and map-edge geometry.
    let candidates = [
        span(0, 12, 24, 12),
        span(12, 0, 12, 24),
        span(0, 0, 24, 24),
        span(0, 24, 24, 0),
        span(0, 1, 24, 19),
        span(1, 0, 19, 24),
        span(0, 0, 0, 24),
        span(24, 0, 24, 24),
    ];
    let groups: Vec<Vec<BoundarySpan>> = candidates
        .iter()
        .copied()
        .map(|candidate| vec![candidate])
        .chain([
            vec![candidates[0], candidates[1]],
            vec![candidates[2], candidates[4]],
            vec![candidates[6], candidates[7], candidates[5]],
        ])
        .collect();

    for mask in 0_u16..(1 << 9) {
        let terrain = grid(3, 3, |x, y| mask & (1 << (y * 3 + x)) != 0);
        for spans in &groups {
            assert_matches_oracle(
                &terrain,
                spans,
                &format!("mask={mask:09b}, spans={spans:?}"),
            );
        }
    }
}

#[test]
fn long_diagonal_near_axis_and_closed_boundary_spans_match_oracle() {
    let spans = [
        span(0, 0, 136, 104),
        span(136, 103, 0, 1),
        span(0, 4, 136, 5),
        span(136, 99, 0, 100),
        span(4, 0, 5, 104),
        span(131, 104, 132, 0),
        span(0, 0, 136, 0),
        span(0, 0, 0, 104),
        span(136, 104, 0, 104),
        span(136, 0, 136, 104),
        span(136, 0, 0, 104),
    ];
    for (case_index, span) in spans.into_iter().enumerate() {
        for terrain in [
            grid(17, 13, |_, _| true),
            grid(17, 13, |x, y| (x * 7 + y * 11) % 5 != 0),
        ] {
            assert_matches_oracle(
                &terrain,
                &[span],
                &format!("long adversarial span {case_index}: {span:?}"),
            );
        }
    }
}

#[test]
fn randomized_medium_fields_match_independent_multi_span_oracle() {
    let mut state = 0xA11C_E5E5_1234_5678_u64;
    for case_index in 0..160 {
        let terrain = grid(17, 13, |_, _| random(&mut state) % 100 < 71);
        let mut spans = vec![
            span(0, 0, 136, 104),
            span(0, 52, 136, 52),
            span(68, 0, 68, 104),
        ];
        for _ in 0..4 {
            let first = PixelPosition {
                x: random(&mut state) % 137,
                y: random(&mut state) % 105,
            };
            let mut second = PixelPosition {
                x: random(&mut state) % 137,
                y: random(&mut state) % 105,
            };
            if second == first {
                second.x = (second.x + 1) % 137;
            }
            spans.push(BoundarySpan {
                endpoints: [first, second],
            });
        }
        assert_matches_oracle(&terrain, &spans, &format!("random case {case_index}"));
    }
}

fn assert_matches_oracle(terrain: &TerrainGrid, spans: &[BoundarySpan], context: &str) {
    let actual = terrain.partition_by_spans(spans).unwrap();
    let expected = oracle_partition(terrain, spans);
    assert_eq!(actual.width(), terrain.width(), "{context}");
    assert_eq!(actual.height(), terrain.height(), "{context}");
    assert_eq!(actual.labels(), expected.labels, "{context}");
    assert_eq!(
        actual
            .areas()
            .iter()
            .map(|area| area.cell_count)
            .collect::<Vec<_>>(),
        expected.areas,
        "{context}"
    );
    assert_eq!(
        actual.boundaries().len(),
        expected.boundaries.len(),
        "{context}"
    );
    for (actual, expected) in actual.boundaries().iter().zip(expected.boundaries) {
        assert_eq!(
            actual.removed_edge_count, expected.removed_edge_count,
            "{context}"
        );
        assert_eq!(
            actual.separated_edge_count, expected.separated_edge_count,
            "{context}"
        );
        assert_eq!(actual.region_pairs, expected.region_pairs, "{context}");
    }
}

fn oracle_partition(terrain: &TerrainGrid, spans: &[BoundarySpan]) -> OraclePartition {
    let edges = legal_edges(terrain);
    let per_span_edges: Vec<Vec<(usize, usize)>> = spans
        .iter()
        .map(|span| {
            edges
                .iter()
                .filter(|edge| span_cuts_edge(*span, edge.left_position, edge.right_position))
                .map(|edge| (edge.left, edge.right))
                .collect()
        })
        .collect();
    let removed: HashSet<_> = per_span_edges.iter().flatten().copied().collect();
    let (labels, areas) = oracle_components(terrain, &edges, &removed);
    let boundaries = per_span_edges
        .iter()
        .map(|edges| {
            let mut region_pairs = Vec::new();
            let mut separated_edge_count = 0;
            for &(left, right) in edges {
                if labels[left] == labels[right] {
                    continue;
                }
                separated_edge_count += 1;
                region_pairs.push([
                    labels[left].min(labels[right]),
                    labels[left].max(labels[right]),
                ]);
            }
            region_pairs.sort_unstable();
            region_pairs.dedup();
            OracleBoundary {
                removed_edge_count: edges.len() as u32,
                separated_edge_count,
                region_pairs,
            }
        })
        .collect();
    OraclePartition {
        labels,
        areas,
        boundaries,
    }
}

/// Rebuilds the legal 8-neighbor graph from public cells. This intentionally does not call the
/// library's private neighbor iterator: diagonal edges require both cardinal side cells walkable.
fn legal_edges(terrain: &TerrainGrid) -> Vec<Edge> {
    let mut edges = Vec::new();
    for y in 0..terrain.height() {
        for x in 0..terrain.width() {
            let left_position = WalkPosition { x, y };
            if !terrain.cell(left_position).unwrap().walkable {
                continue;
            }
            for (delta_x, delta_y) in [(1_i32, 0_i32), (0, 1), (1, 1), (-1, 1)] {
                let Some(right_x) = x.checked_add_signed(delta_x) else {
                    continue;
                };
                let Some(right_y) = y.checked_add_signed(delta_y) else {
                    continue;
                };
                let right_position = WalkPosition {
                    x: right_x,
                    y: right_y,
                };
                let Some(right) = terrain.cell(right_position) else {
                    continue;
                };
                if !right.walkable {
                    continue;
                }
                if delta_x != 0 && delta_y != 0 {
                    let side_a = terrain.cell(WalkPosition { x: right_x, y });
                    let side_b = terrain.cell(WalkPosition { x, y: right_y });
                    if !side_a.is_some_and(|cell| cell.walkable)
                        || !side_b.is_some_and(|cell| cell.walkable)
                    {
                        continue;
                    }
                }
                edges.push(Edge {
                    left: index(terrain.width(), left_position),
                    right: index(terrain.width(), right_position),
                    left_position,
                    right_position,
                });
            }
        }
    }
    edges
}

fn oracle_components(
    terrain: &TerrainGrid,
    edges: &[Edge],
    removed: &HashSet<(usize, usize)>,
) -> (Vec<u32>, Vec<u32>) {
    let mut adjacency = vec![Vec::new(); terrain.cells().len()];
    for edge in edges {
        if removed.contains(&(edge.left, edge.right)) {
            continue;
        }
        adjacency[edge.left].push(edge.right);
        adjacency[edge.right].push(edge.left);
    }
    let mut labels = vec![0; terrain.cells().len()];
    let mut areas = Vec::new();
    let mut queue = VecDeque::new();
    for start in 0..labels.len() {
        if !terrain.cells()[start].walkable || labels[start] != 0 {
            continue;
        }
        let label = areas.len() as u32 + 1;
        labels[start] = label;
        queue.push_back(start);
        let mut count = 0;
        while let Some(current) = queue.pop_front() {
            count += 1;
            for &next in &adjacency[current] {
                if labels[next] == 0 {
                    labels[next] = label;
                    queue.push_back(next);
                }
            }
        }
        areas.push(count);
    }
    (labels, areas)
}

/// Exact finite segment intersection through parametric cross products. It is deliberately
/// independent from the library implementation and keeps endpoint intersections closed.
fn span_cuts_edge(span: BoundarySpan, first: WalkPosition, second: WalkPosition) -> bool {
    let [one, two] = span.endpoints;
    let (a, b) = if one <= two { (one, two) } else { (two, one) };
    let first = center(first);
    let second = center(second);
    let first_side = cross(a, b, first);
    let second_side = cross(a, b, second);
    (first_side >= 0) != (second_side >= 0) && finite_segments_intersect(a, b, first, second)
}

fn finite_segments_intersect(
    first_start: PixelPosition,
    first_end: PixelPosition,
    second_start: PixelPosition,
    second_end: PixelPosition,
) -> bool {
    let first_direction = vector(first_start, first_end);
    let second_direction = vector(second_start, second_end);
    let displacement = vector(first_start, second_start);
    let denominator = vector_cross(first_direction, second_direction);
    if denominator == 0 {
        return vector_cross(displacement, first_direction) == 0
            && boxes_overlap(first_start.x, first_end.x, second_start.x, second_end.x)
            && boxes_overlap(first_start.y, first_end.y, second_start.y, second_end.y);
    }
    let first_numerator = vector_cross(displacement, second_direction);
    let second_numerator = vector_cross(displacement, first_direction);
    within_closed_unit(first_numerator, denominator)
        && within_closed_unit(second_numerator, denominator)
}

fn within_closed_unit(numerator: i64, denominator: i64) -> bool {
    if denominator > 0 {
        (0..=denominator).contains(&numerator)
    } else {
        (denominator..=0).contains(&numerator)
    }
}

fn boxes_overlap(first_a: u32, first_b: u32, second_a: u32, second_b: u32) -> bool {
    first_a.min(first_b) <= second_a.max(second_b) && second_a.min(second_b) <= first_a.max(first_b)
}

fn vector(from: PixelPosition, to: PixelPosition) -> (i64, i64) {
    (to.x as i64 - from.x as i64, to.y as i64 - from.y as i64)
}

fn vector_cross(left: (i64, i64), right: (i64, i64)) -> i64 {
    left.0 * right.1 - left.1 * right.0
}

fn cross(a: PixelPosition, b: PixelPosition, point: PixelPosition) -> i64 {
    vector_cross(vector(a, b), vector(a, point))
}

fn center(position: WalkPosition) -> PixelPosition {
    PixelPosition {
        x: position.x * PIXELS_PER_CELL as u32 + 4,
        y: position.y * PIXELS_PER_CELL as u32 + 4,
    }
}

fn index(width: u32, position: WalkPosition) -> usize {
    (position.y * width + position.x) as usize
}

fn grid(width: u32, height: u32, mut walkable: impl FnMut(u32, u32) -> bool) -> TerrainGrid {
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

fn random(state: &mut u64) -> u32 {
    *state = state
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1);
    (*state >> 32) as u32
}
